(ns fermor.force-atlas.matrix
  (:require [fermor.core :as g]
            [clojure.core.matrix :as cm :refer :all]
            [clojure.core.matrix.linear :as l]
            [clojure.core.reducers :as r]
            [fastmath.core :as fm :refer [atan2]]
            [fastmath.vector :as fmv]))

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

(defn rand-ints [n]
  (take n (iterate (fn [_] (rand-int 100)) (rand-int 100))))


(defn graph-from-triples [triples]
  (let [edges (map-indexed
                (fn [eid [from w to]]
                  [from to eid])
                triples)
        g (-> (g/graph)
            (g/add-edges :to edges)
            g/forked)
        vc (count (g/vertices g [:to]))
        ec (count triples)
        g
        ;; Metadata should be resilient on the graph object.
        (with-meta g {:vertices (cm/zero-array :vectorz [vc 4])
                      :positions (doto (cm/zero-array :vectorz [vc 2])
                                   (cm/set-column! 0 (rand-ints vc))
                                   (cm/set-column! 1 (rand-ints vc)))
                      :edges (doto (cm/zero-array :vectorz [ec 5])
                               (cm/set-column! 0 (map second triples))
                               (cm/set-column! 3 (map first triples))
                               (cm/set-column! 4 (map last triples)))
                      :vc vc :ec ec})]
    g))

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

(defn dists-from-center [g]
  (->> (vp g) cm/rows (mapv cm/magnitude)))

(defn edge-dimensions [g]
  (let [vm (vm g)
        em (em g)
        vp (vp g)
        eids (range (ec g))]
    (let [from-idxs (cm/get-column em 3)
          to-idxs (cm/get-column em 4)
          from-pos (cm/select vp from-idxs :all)
          to-pos (cm/select vp to-idxs :all)
          edge-dimensions (fn edge-dimensions [eid]
                            (let [fp (cm/get-row from-pos (long (cm/select from-idxs eid)))
                                  tp (cm/get-row to-pos (long (cm/select to-idxs eid)))
                                  ev (cm/sub fp tp)
                                  dist (l/norm ev)
                                  angle (atan2 (cm/mget ev 0) (cm/mget ev 1))]
                             [dist angle]))]
      (cm/matrix :vectorz
        ;; TODO: tune
        (if (< (ec g) 1024)
          (into [] (map edge-dimensions) eids)
          (into [] (r/map edge-dimensions eids)))))))

(defn- mutating [x]
  (fn
    ([] x)
    ([ctor]
     (fn
       ([] (ctor))
       ([left right] x)))
    ([a b] x)))

(defn v*v-spacial-relationship [g]
  (let [vp (vp g)
        vc (vc g)
        xjoin (cm/zero-matrix :vectorz (* vc vc) 8)]
    (doseq [i (range vc)]
      (cm/assign! (cm/submatrix xjoin [[(* vc i) vc] [0 2]])
        (cm/get-row vp i))
      (cm/assign! (cm/submatrix xjoin [[(* vc i) vc] [2 2]])
        vp))
    (if false
      (r/fold (mutating xjoin)
        (fn [_ i]
          (let [row (cm/get-row xjoin i)
                fp (cm/submatrix row [[0 2]])
                tp (cm/submatrix row [[2 2]])
                diff (cm/sub fp tp)
                dist (cm/distance fp tp)]
            (cm/mset! row 4 dist)
            (cm/mset! row 5 (cm/mget diff 0))
            (cm/mset! row 6 (cm/mget diff 1))
            xjoin))
        (range (* vc vc)))
      (dotimes [i (* vc vc)]
        (let [row (cm/get-row xjoin i)
              fp (cm/submatrix row [[0 2]])
              tp (cm/submatrix row [[2 2]])
              diff (cm/sub fp tp)
              dist (cm/distance fp tp)]
          (cm/mset! row 4 dist)
          (cm/mset! row 5 (cm/mget diff 0))
          (cm/mset! row 6 (cm/mget diff 1)))))))

(into (vec (cm/array :vectorz [1 2])) [10])
(time
  (v*v-spacial-relationship g))


(do
  (time (do (edge-dimensions g) nil)))

(defn update-edge-dimensions! [g]
  (let [d (edge-dimensions g)
        em (em g)]
    (set-column! em 1 (get-column d 0))
    (set-column! em 2 (get-column d 1))))

(defn shapes [sides g]
  (g/descend []
      (fn [path e]
       (let [r (cond
                (and (= (count path) sides) (= e (first path))) g/emit-and-continue
                (< sides (count path)) g/ignore
                (some #(= e %) path) g/ignore
                :else g/continue)]
        r))
      (fn [path e] (g/both [:to] e))
      (g/with-paths (g/vertices g))))

;; a pair of triangles that share an edge also look like a square, so to find only squares I have to
;; remove any squares that contain all of the elements of any triangle
;;
(defn squares [g]
  (let [sq (shapes 4 g)
        tri (set (map #(fermor.path/subpath % 2) (shapes 3 g)))]
    (remove #(tri (fermor.path/subpath % 4)) sq)))

;;  #path/V [(v 139) <-:to- (v 140) -:to-> (v 127) <-:to- (v 133) <-:to- (v 139)]

(count (squares g))
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
