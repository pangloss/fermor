(ns fermor.force-atlas
  (:require [fermor.core :as g]
            [fermor.force-atlas.graph :refer :all]
            [clojure.core.reducers :as r]
            [fastmath.core :as fm]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(fm/use-primitive-operators)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; Good graph layout info is hard to find. The best I found was in this presentation:
;; https://gephi.org/tutorials/gephi-tutorial-layouts.pdf
;; This is ported from the ForceAtlas2 implementation in Gephi.
;; Interesting related material: (I've downloaded in case this flaky looking url goes away)
;; http://asus.myds.me:6543/paper/nw/Exploiting%20GPUs%20for%20fast%20force-directed%20visualization%20of%20large-scale%20networks.pdf
;; Note that there are several configurable algorithmic paths and I only chose one. I later discovered that those choices were not
;; the default settings, so may be providing suboptimal performance.
;; The algo is also not thoroughly verified against the original. It does function, but I have not compared quality of results.
;; other interesting work: http://mgarland.org/files/papers/layoutgpu.pdf

;; The force atlas algo has 4 forces:
;; - friction
;; - gravity
;; - vertex repulsion (sometimes with anti-collision)
;; - edge pull
;;
;; I would like to add a phase which induces rotation of formations such that the graph
;; becomes self-leveling. Perhaps edges could have some sort of buoyancy?
;;
;; Maybe grid structures should always feel the forces from all of the other
;; nodes in the grid structure? That seems to encourage unfolding if I crank up
;; the local-repulsion factor to about 100 or 200...

(defn apply-deltas [v ^double speed ^double friction]
  (let [vel (velocity v) ;; dx ...
        vel (v/rotate vel (* 0.05 ^double (rand)))
        accel (v/sub (prev-velocity v) vel)  ;; ddx...
        swinging (* ^double (v-mass v) (v/mag accel))
        factor (/ speed (+ 1.0 (fm/sqrt (* speed swinging))))]
    (swap-position v v/add (v/mult vel factor))
    (set-prev-velocity v vel)
    (set-velocity v (v/mult vel friction))))

(defn ->apply-force [force! ^double coefficient]
  (fn apply-force [vertex-docs node]
    (let [pos (position node)]
      (doseq [n vertex-docs]
        (let [distance (v/dist pos (position n))]
          ;; if nodes are super close together, the repulsion forces go in random directions and
          ;; clumps will get locked together in little balls.
          (when (< 0.0001 distance)
            (force! node n coefficient)))))))

(defn local-repulsion [node other ^double coefficient]
  (let [lr 40.0
        dv (v/sub (position node) (position other))
        distance (max 0.01 (v/mag dv))]
    (when (and (< distance lr) (pos? distance))
      (let [factor (/ (* coefficient (* (v-mass node) (v-mass other)))
                     (/ (* distance distance) 20))
            push (v/mult dv factor)]
        (swap-velocity node v/add push)
        (swap-velocity other v/sub push)))))


(defn lin-repulsion [node other ^double coefficient]
  (let [dv (v/sub (position node) (position other))
        distance (max 0.01 (v/mag dv))
        factor (/ (* coefficient (* (v-mass node) (v-mass other)))
                 (* distance distance) 0.5)
        push (v/mult dv factor)]
    (swap-velocity node v/add push)
    (swap-velocity other v/sub push)))

(defn lin-attraction [out-v ^double edge-weight-influence ^double coeff]
  (let [n1 (doc out-v)]
    (doseq [e (g/out-edges out-v [:to])
            :let [n2 (doc (g/in-vertex e))
                  edge (g/get-document e)
                  sq (if (or (pos? (v-squares n1)) (pos? (v-squares n2))) 2.0 1.0)
                  edge-weight (fm/pow (* sq (e-weight edge))
                                edge-weight-influence)
                  edge-length (length edge)
                  push (v/mult (edge-vector edge) (* edge-weight coeff))]]
      ;; if distance is too short, push the edge back off
      (if (< 0.01 ^double edge-length)
        (do ;; do the usual thing
          (swap-velocity n1 v/sub push)
          (swap-velocity n2 v/add push))
        (do ;; do the opposite: push away
          (swap-velocity n1 v/add push)
          (swap-velocity n2 v/sub push))))))

(defn strong-gravity [node ^double gravity*coefficient]
  (when-not (zero? gravity*coefficient)
    (let [factor (* gravity*coefficient ^double (v-mass node))
          gravity (v/emult (position node) (v/vec2 factor (* factor 0.75 #_ 0.33)))]
      (swap-velocity node v/add gravity))))

(defn calculate-tuning-info [^doubles arr v]
  (let [swinging (aget arr 0)
        traction (aget arr 1)
        vel (velocity v)
        prev-vel (prev-velocity v)
        mass (v-mass v)]
    (doto (double-array 2)
      (aset 0 (+ swinging (* mass (v/dist prev-vel vel))))
      (aset 1 (+ traction (* 0.5 mass
                            (v/mag (v/add prev-vel vel))))))))

(defn force-atlas [graph]
  ;; TODO:
  ;; - with a larger graph, some phases need more time. Especially the gravity phase can take longer if the net spread out further. I think
  ;;   gravity should run until the (/ (sqrt traction) vc) or (/ traction (pow vc 1.8)) number gets to about 1.0 or less. The sqrt version
  ;;   seems a bit more jitter resistant?
  (let [{:keys [^long vc ^long ec ^long iter ^double speed ^double speed-efficiency ^long traction ^double default-gravity]
         :or {iter 0 speed 1.0 speed-efficiency 1.0}}
        (meta graph)
        traction (or traction (* 3500 vc))
        edge-weight-influence (if (< iter 250) 0.0 0.5)
        friction (-> (- 1 (* 0.05 (/ traction (fm/pow vc 1.8))))
                   (min 0.95)
                   (max 0.2))
        extend 0
        gravity (if (< 150 iter (+ extend 450))
                  (* 2 4 -0.005)
                  (or default-gravity 0.0))
        coefficient 25.0
        g*coeff (* gravity coefficient)
        repulsion-force! (->apply-force
                           (if (< -1 iter 250)
                             lin-repulsion
                             local-repulsion)
                           (* coefficient (max 0.2 (min (/ 50.0 iter) 4.0))))
        vs (into [] (g/vertices graph))
        vds (into [] (g/documents vs))]
    ;; I use r/fold to do a parallelized iteration through the vertices and edges
    (r/fold
      (fn ([] nil) ([a b] nil))
      (fn [_ v]
        (repulsion-force! vds v)
        (strong-gravity v g*coeff))
      vds)
    (r/fold
      (fn ([] nil) ([a b] nil))
      (fn [_ v] (lin-attraction v edge-weight-influence coefficient))
      vs)
    (let [^doubles arr (reduce calculate-tuning-info (double-array [0.0 0.0]) vds)
          swinging (aget arr 0)
          traction (aget arr 1)
          jitter-tolerance 0.02
          chaos-threshold (* jitter-tolerance
                            (min 10.0 (* (* 0.05 (fm/sqrt vc))
                                        (/ traction
                                          (fm/sq vc)))))
          min-speed-efficiency 0.05
          ;; protection against erratic behavior
          [^double speed-efficiency ^double chaos-threshold]
          (if (< 2.0 (/ swinging traction)) ;; swinging without as much traction
            [(if (< min-speed-efficiency speed-efficiency) ;; above min speed
               (* speed-efficiency 0.5) ;; slow down a lot
               speed-efficiency)
             (max chaos-threshold jitter-tolerance)]
            [speed-efficiency chaos-threshold])
          target-speed (* chaos-threshold speed-efficiency (/ traction swinging))
          speed-efficiency
          (cond (< (* chaos-threshold traction) swinging) ;; too much chaos
                (if (< min-speed-efficiency speed-efficiency) ;; and above minimum speed
                 (* speed-efficiency 0.7) ;; slow down
                 speed-efficiency)
                (< 1000 speed) ;; not moving too fast
                (* speed-efficiency 1.3) ;; speed up
                :else speed-efficiency)
          max-rise 0.5
          speed (+ speed (min (- target-speed speed) (* max-rise speed)))]
      (r/fold
        (fn ([] nil) ([a b] nil))
        (fn [_ v]
          (apply-deltas v speed friction))
        vds)
      (update-edge-documents graph)
      (with-meta
        graph
        {:vc vc :ec ec
         :speed speed :speed-efficiency speed-efficiency :swinging swinging
         :traction traction :f (/ traction (fm/pow vc 1.8))
         :iter (inc iter) :friction friction
         :default-gravity default-gravity
         :gravity gravity}))))
