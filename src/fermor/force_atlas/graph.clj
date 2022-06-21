(ns fermor.force-atlas.graph
  (:require [fermor.core :as g]
            [fastmath.core :as fm :refer [atan2]]
            [fastmath.vector :as fv :refer [vec2 dist]]
            [untether.ugf :as ugf]
            [clojure.set :as set]))

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


(defn update-edge-documents [g]
  (g/forked
    (reduce (fn [lg e]
              (let [from-doc (g/get-document (g/out-vertex e))
                    to-doc (g/get-document (g/in-vertex e))
                    fv @(:pos from-doc)
                    tv @(:pos to-doc)
                    ev (fv/sub fv tv)
                    edoc (g/get-document e)]
                (vreset! (:distance edoc) (fv/mag ev))
                (vreset! (:angle edoc) (fv/heading ev))))
      (g/linear g)
      (g/out-e [:to] (g/vertices g)))))



(defn attach-edge-documents [g]
  (g/forked
    (reduce (fn [lg e]
              (let [from-doc (g/get-document (g/out-vertex e))
                    to-doc (g/get-document (g/in-vertex e))
                    fv @(:pos from-doc)
                    tv @(:pos to-doc)
                    ev (fv/sub fv tv)]
                (g/set-document lg e
                  {:distance (volatile! (fv/mag ev))
                   :angle (volatile! (fv/heading ev))})))
      (g/linear g)
      (g/out-e [:to] (g/vertices g)))))


(defn make-graph [triples]
  (->> triples
    graph-from-triples
    attach-vertex-documents
    attach-edge-documents))

(def g (make-graph triples))

(defn em [g] (get (meta g) :edges))
(defn vm [g] (get (meta g) :vertices))
(defn vp [g] (get (meta g) :positions))
(defn vc [g] (get (meta g) :vc))
(defn ec [g] (get (meta g) :ec))

(defn v-x [vp v] (cm/mget vp (g/get-document v) 0))
(defn v-y [vp v] (cm/mget vp (g/get-document v) 1))
(defn v-pos [vp v] (cm/get-row vp (g/get-document v)))
(defn v-dx [vm v] (cm/mget vm (g/get-document v) 0))
(defn v-dy [vm v] (cm/mget vm (g/get-document v) 1))
(defn v-centrality [vm v] (cm/mget vm (g/get-document v) 2))
(defn v-squares [vm v] (cm/mget vm (g/get-document v) 3))

(defn e-weight [em e] (cm/mget em (g/get-document e) 0))
(defn e-length [em e] (cm/mget em (g/get-document e) 1))
(defn e-angle [em e] (cm/mget em (g/get-document e) 2))
(defn e-from [em e] (long (cm/mget em (g/get-document e) 3)))
(defn e-to [em e] (long (cm/mget em (g/get-document e) 4)))

(defn e-from* [e]
  (g/element-id (g/in-vertex e)))

(defn e-to* [e]
  (g/element-id (g/out-vertex e)))

(defn update-edge-dimensions! [g]
  (let [d (edge-dimensions g)
        em (em g)]
    (set-column! em 1 (get-column d 0))
    (set-column! em 2 (get-column d 1))))

(def triples (map (fn [[from e to]]
                    [(ugf/id from) (rand-int 20) (ugf/id to)])
               (ugf/triples (ugf/read-ugf "/Users/dw/Downloads/bert-297.ugf"))))

(def g (graph-from-triples triples))

;;  #path/V [(v 139) <-:to- (v 140) -:to-> (v 127) <-:to- (v 133) <-:to- (v 139)]

(count (squares g))

(sort-by val (squares g))

(map #(fermor.path/subpath % 2) (shapes 3 g))

(filter #(= 140 (g/element-id %))
  (concat
    (shapes 3 g)
    (shapes 4 g)))

(count (shapes 4 g))

(= (seq (g/path (first sqs))) (g/reverse-path (g/path (nth sqs))))

(g/both (g/get-vertex g 122))

(take 50 (g/both (g/both (g/with-paths (g/vertices g)))))

(g/vertices g)

triples

(g/both [(g/get-vertex g 0)])
(map (juxt identity g/both) (g/vertices g))
