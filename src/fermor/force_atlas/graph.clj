(ns fermor.force-atlas.graph
  (:require [fermor.core :as g]
            [fastmath.core :as fm :refer [atan2]]
            [fastmath.vector :as fv :refer [vec2 dist]]
            [untether.ugf :as ugf]
            [clojure.set :as set]
            [clojure.core.reducers :as r])
  (:import [fastmath.vector Vec2]))

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

(defn shapes [sides g]
  (g/descend []
      (fn [path e]
        (let [r (cond
                  ;; match
                  (and (= (count path) sides) (= e (first path))) g/emit-and-continue
                  ;; path too long
                  (< sides (count path)) g/ignore
                  ;; originate shapes on out-edges only. Prevents every shape being counted twice.
                  (and (= 1 (count path)) (g/followed-reverse? (second (g/path e)))) g/ignore
                  ;; duplicated element in path: invalid
                  (some #(= e %) path) g/ignore
                  ;; keep searching this path
                  :else g/continue)]
          r))
      (fn [path e] (g/both [:to] e))
      (g/with-paths (g/vertices g))))

(defn squares [g]
  ;; a pair of triangles that share an edge also look like a square, so to find only squares I have to
  ;; remove any squares that contain all of the elements of any triangle
  (let [tri (into #{}
              (map #(set (map g/element-id (remove g/edge? (g/path %)))))
              (shapes 3 g))]
    (->> (shapes 4 g)
      (remove #(tri (set (map g/element-id (remove g/edge? (g/subpath % 4))))))
      (map g/no-path)
      frequencies)))

(defn graph-from-triples [triples]
  (let [edges (map-indexed
                (fn [eid [from w to]]
                  [from to eid])
                triples)
        g (-> (g/graph)
            (g/add-edges :to edges)
            g/forked)
        vc (count (g/vertices g [:to]))
        ec (count triples)]
    (with-meta g {:vc vc :ec ec})))

(defn attach-vertex-documents [g]
  (let [sqs (squares g)]
    (g/forked
      (reduce (fn [lg v]
                (g/set-document lg v
                  {:id (g/element-id v)
                   :pos (volatile! (vec2 (rand-int 100) (rand-int 100)))
                   :move (volatile! (vec2 0 0))
                   :size 1
                   :degree (g/degree v)
                   :squares (sqs v 0)}))
        (g/linear g)
        (g/vertices g)))))

(defn attach-edge-documents [g]
  (g/forked
    (reduce (fn [lg e]
              (g/set-document lg e
                {:length (volatile! 0)
                 :angle (volatile! 0)}))
      (g/linear g)
      (g/out-e [:to] (g/vertices g)))))

(defn update-edge-documents [g]
  (r/fold
    (fn ([] g) ([_ __] g))
    (fn [g e]
      (let [from-doc (g/get-document (g/out-vertex e))
            to-doc (g/get-document (g/in-vertex e))
            fv @(:pos from-doc)
            tv @(:pos to-doc)
            ev (fv/sub fv tv)
            edoc (g/get-document e)]
        (vreset! (:length edoc) (fv/mag ev))
        (vreset! (:angle edoc) (fv/heading ev))
        g))
    (g/out-e [:to] (g/vertices g))))


(defn make-graph [triples]
  (->> triples
    graph-from-triples
    attach-vertex-documents
    attach-edge-documents
    update-edge-documents))

(defn em [g] (get (meta g) :edges))
(defn vm [g] (get (meta g) :vertices))
(defn vp [g] (get (meta g) :positions))
(defn vc [g] (get (meta g) :vc))
(defn ec [g] (get (meta g) :ec))

(defn v-pos ^Vec2 [v] @(:pos (g/get-document v)))
(defn v-x [v] (.x (v-pos v)))
(defn v-y [v] (.y (v-pos v)))
(defn v-move ^Vec2 [v] @(:move (g/get-document v)))
(defn v-dx [v] (.x (v-move v)))
(defn v-dy [v] (.y (v-move v)))
(defn v-degree [v] (:degree (g/get-document v)))
(defn v-squares [vm v] (:squares (g/get-document v)))

(defn e-weight [e] (:weight (g/get-document e)))
(defn e-length [e] (:length (g/get-document e)))
(defn e-angle [e] (:angle (g/get-document e)))

(comment
  (def triples (map (fn [[from e to]]
                      [(ugf/id from) (rand-int 20) (ugf/id to)])
                 (ugf/triples (ugf/read-ugf "/Users/dw/Downloads/bert-297.ugf"))))

  (def g (make-graph triples)))
