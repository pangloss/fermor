(ns fermor.force-atlas
  (:require [fermor.core :as g]
            [fermor.force-atlas.graph :refer :all]
            [clojure.core.reducers :as r]
            [fastmath.core :as fm]
            [fastmath.vector :as v]
            [untether.ugf :as ugf])
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
;; The algo seems to work best with gravity turned off. Previously I'd get a nice state
;; with gravity off, then slowly ramp it back on to make the graph more dense again.
;; I think I can leave gravity off if I instead increase the edge pull.
;;
;; Increasing edge pull may have some nice possibilities. For instance I can identify
;; patterns in the graph and start the edge pull earlier or later on them, or make them
;; stronger or weaker.
;;
;; I would like to add a phase which induces rotation of formations such that the graph
;; becomes self-leveling. Perhaps edges could have some sort of buoyancy?
;;
;; Finally, I think the edge grouping is an optimization that is CPU specific and if I
;; move to matrix operations it may be more efficient to operate simply on nodes directly.
;; But even if not I think the grouping aspect is basically an add-on.
;;
;; There are a few oddball values that also get calculated like "traction",
;; which I use to wangle out and adjustment to the friction per-frame.

(defn apply-deltas [v ^double speed ^double friction]
  (let [vel (velocity v) ;; dx ...
        vel (v/rotate vel (* 0.05 (rand)))
        #_#_
        vel (v/sub vel
              ;; introduce a little noise to kill oscilations
              (v/emult vel
                (v/mult (v/generate-vec2 rand) 0.10)))
        accel (v/sub (prev-velocity v) vel)  ;; ddx...
        swinging (* ^double (v-mass v) (v/mag accel))
        factor (/ speed (+ 1.0 (fm/sqrt (* speed swinging))))]
    (swap-position v v/add (v/mult vel factor))
    (set-prev-velocity v vel)
    (set-velocity v (v/mult vel friction))))

(defn ->apply-force [force! ^double theta ^double coefficient]
  (fn apply-force [vertex-docs node]
    (let [pos (position node)]
      (doseq [n vertex-docs]
        (let [distance (v/dist pos (position n))]
          ;; if nodes are super close together, the repulsion forces go in random directions and
          ;; clumps will get locked together in little balls.
          (when (< (v-size n) (* distance theta))
            (force! node n coefficient)))))))

(defn local-repulsion [node other ^double coefficient]
  (let [lr 30.0
        dv (v/sub (position node) (position other))
        distance (max 0.01 (v/mag dv))]
    (when (and (< distance lr) (pos? distance))
      (let [factor (/ (* coefficient (* (v-mass node) (v-mass other)))
                     (/ distance 2.0))
            push (v/mult dv factor)]
        (swap-velocity node v/add push)
        (swap-velocity other v/sub push)))))

(defn lin-repulsion [node other ^double coefficient]
  (let [dv (v/sub (position node) (position other))
        distance (max 0.01 (v/mag dv))
        factor (/ (* coefficient (* (v-mass node) (v-mass other))
                    (/ distance 2)))
        push (v/mult dv factor)]
    (swap-velocity node v/add push)
    (swap-velocity other v/sub push)))

(defn lin-attraction [out-v ^double edge-weight-influence ^double neg-coeff ^double theta]
  (let [n1 (doc out-v)]
    (doseq [e (g/out-edges out-v [:to])
            :let [n2 (doc (g/in-vertex e))
                  edge (g/get-document e)
                  edge-weight (fm/pow (e-weight edge) edge-weight-influence)
                  too-close (* ^double (length edge) (* 5.0 theta))
                  pull (v/mult (edge-vector edge) (* edge-weight neg-coeff))]]
      ;; if distance is too short, push the edge back off
      (if (and (< (v-size n1) too-close)
            (< (v-size n2) too-close))
        (do ;; do the usual thing
          (swap-velocity n1 v/add pull)
          (swap-velocity n2 v/sub pull))
        (do ;; do the opposite: push away
          (swap-velocity n1 v/sub (v/mult pull 1.0))
          (swap-velocity n2 v/sub (v/mult pull 1.0)))))))

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
  (let [{:keys [^long vc ^long ec ^long iter ^double speed ^double speed-efficiency ^long traction]
         :or {iter 0 speed 1.0 speed-efficiency 1.0}}
        (meta graph)
        traction (or traction (* 3500 vc))
        edge-weight-influence (if (< iter 250) 0.0 0.5)
        friction (-> (- 1 (* 0.05 (/ traction (fm/pow vc 1.8))))
                   (min 0.95)
                   (max 0.2))
        gravity (if (< 150 iter 450)
                  (* 4 -0.005)
                  0.0)
        coefficient 25.0
        neg-coeff (- coefficient)
        g*coeff (* gravity coefficient)
        theta 0.4
        repulsion-force! (->apply-force
                           (if (< -1 iter 250)
                             lin-repulsion
                             local-repulsion)
                           theta
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
      (fn [_ v] (lin-attraction v edge-weight-influence neg-coeff theta))
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
      ;; TODO enable:
      (update-edge-documents graph)
      (with-meta
        graph
        {:vc vc :ec ec
         :speed speed :speed-efficiency speed-efficiency :swinging swinging
         :traction traction :f (/ traction (fm/pow vc 1.8))
         :iter (inc iter) :friction friction
         :gravity gravity}))))
