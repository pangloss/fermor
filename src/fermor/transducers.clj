(ns fermor.transducers
  (:require [pure-conditioning :refer [condition manage lazy-conditions error default]]
            [potemkin :refer [import-vars import-def]]
            [flatland.ordered.set :refer [ordered-set]]
            [fermor.protocols :as proto :refer [wrappable? Wrappable -out-edges -in-edges
                                                -out-edge-count -in-edge-count
                                                to-forked to-linear
                                                traversed-forward -label -unwrap
                                                -out-edges-prepared -in-edges-prepared
                                                -transpose -has-vertex? -get-edge]]
            [fermor.descend :refer [*descend *descents extrude *no-result-interval*]]
            [fermor.graph :as g
             :refer [out-edges-prepared3 in-edges-prepared3]]
            [fermor.kind-graph :refer [->KGraph]]
            fermor.path
            [xn.transducers :as tx]
            [fermor.core :refer [ensure-seq go-back go-on]])
  (:import clojure.lang.IMeta
           (io.lacuna.bifurcan LinearList Lists)
           (fermor.protocols TraversalDirection KindId)))


(import-vars (fermor.protocols set-config!
               ;; Graph
               get-vertex all-vertices all-edges
               ;; Predicates
               graph? vertex? edge? element? linear? forked? path?
               ;; MutableGraph
               add-vertices add-edges set-documents
               remove-vertices remove-edges remove-documents
               ;; Element
               element-id get-document get-graph exists?
               ;; Edge
               out-vertex in-vertex
               ;; Path
               reverse-path
               ;; KindId
               id k kind lookup)
  ;; Bifurcan Graph
  (fermor.graph dag-edge digraph-edge
    undirected-edge build-graph vertices-with-edge
    ;; read printed graph elements
    v e-> e->in
    -v -e-> -e->in)
  ;; Path
  (fermor.path with-path path subpath no-path no-path! cyclic-path?
    path-vertices path-edges)
  ;; Kind Graph
  (fermor.kind-graph V E-> E->in))

(defn fast-trav
  "Steps to traverse a node's edges:
    - once per route:
        - get the node's graph
        - get the matching set of edge-graphs for the graph
    - look up the node in each edge-graph to find its edges
    - catenate the results"
  [get-edges labels]
  (fn [rf]
    (let [edge-graphs (volatile! [])
          labels (if (keyword? labels) [labels] (ensure-seq labels))
          w (volatile! nil)
          w-init (fn [vertex]
                   (let [g (get-graph vertex)
                         edge-graphs (into [] (map #(g/edge-graph g %)) labels)
                         ;; create a fully initialized worker
                         w' (fn [vertex]
                              (loop [ls labels egs edge-graphs
                                     r (LinearList.)]
                                (if (seq ls)
                                  (Lists/concat r (get-edges g (first ls) (first egs) vertex))
                                  (.forked r))))]
                     ;; replace this fn with the initialized worker
                     (vreset! w w')
                     ;; actually do the work
                     (w' vertex)))
          work (fn [v] (@w v))]
      (vreset! w w-init)
      (fn
        ;; init
        ([] (rf))
        ;; complete
        ([result] (rf result))
        ;; reduce item
        ([result input]
         (rf result (work input)))))))

(def with-paths (map with-path))
(def reverse-paths (map path))
(def paths (map path))

(defn in-e*
  ([] (map -in-edges))
  ;; TODO: in-edges-prepared3 should be a protocol with all extensibility features tied in.
  ([labels] (fast-trav in-edges-prepared3 labels)))

(defn in-e
  ([] (comp (in-e*) cat))
  ([labels] (comp (in-e* labels) cat)))

(defn out-e*
  ([] (map -out-edges))
  ([labels] (fast-trav out-edges-prepared3 labels)))

(defn out-e
  ([] (comp (out-e*) cat))
  ([labels] (comp (out-e* labels) cat)))

(defn cat-each [& xforms]
  (fn [rf]
    (let [xfs (mapv #(% conj) xforms)]
      (fn
        ([] (doseq [f xfs] (f)))
        ([result]
         (rf result))
        ([result input]
         (rf result (reduce (fn [v xf] (xf v input)) [] xfs)))))))

(defn both-e*
  ([] (map (fn [v] (concat (-in-edges v) (-out-edges v)))))
  ([labels] (cat-each (in-e* labels) (out-e* labels))))

(defn both-e
  ([] (comp (both-e*) cat))
  ([labels] (comp (both-e* labels) cat)))

(def out-v (map out-vertex))
(def in-v (map in-vertex))

(def other-v (map go-on))
(def same-v (map go-back))

(def both-v
  "Returns a lazy seq of vertices out of a collection of edges."
  (mapcat #(vector (in-vertex %) (out-vertex %))))

(defn mapmap [f]
  (map #(mapv f %)))

(defn in*
  ([] (comp in-e* (mapmap out-vertex)))
  ([labels] (comp (in-e* labels) (mapmap out-vertex))))

(defn in
  ([] (comp in-e* out-v))
  ([labels] (comp (in-e labels) out-v)))

(defn out*
  ([] (comp out-e* (mapmap in-vertex)))
  ([labels] (comp (out-e* labels) (mapmap in-vertex))))

(defn out
  ([] (comp out-e* in-v))
  ([labels] (comp (out-e labels) in-v)))

(defn in-sorted [labels sort-by-f]
  (comp (in* labels) (map #(sort-by sort-by-f %)) cat))

(defn out-sorted [labels sort-by-f]
  (comp (out* labels) (map #(sort-by sort-by-f %)) cat))

(def documents
  (map get-document))


;; TODO: move to transducers lib
(defn lookahead
  "Uses a nested transducer as the lookahead body"
  ([xform]
   (fn [rf]
     (let [look (xform (fn ([] nil) ([_] nil) ([_ item] (reduced true))))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result item]
          (if (look nil item)
            (rf result item)
            result))))))
  ([{:keys [min max]} xform]
   (fn [rf]
     (let [finds (volatile! 0)
           look (xform (fn
                         ([] nil)
                         ([_] nil)
                         ([_ item]
                          ;; this gets called only when an item would be added to the collection
                          (vswap! finds inc))))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result item]
          (vreset! finds 0)
          (look nil item)
          (if (<= min @finds max)
            (rf result item)
            result)))))))

;; TODO: move to transducers lib
(defn neg-lookahead
  "Ensure that the function does NOT produce a collection of at least one item.

   Use the arity 2 version to specify that there must NOT be at least min
   and/or at most max items in the route. If min or max is nil that limit will
   not be enforced. The arity 2 version of neg-lookahead is not really recommended
   as it is a little bit confusing."
  ([xform]
   (fn [rf]
     (let [look (xform (fn ([] nil) ([_] nil) ([_ item] (reduced true))))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result item]
          (if (look nil item)
            result
            (rf result item)))))))
  ([{:keys [min max]} xform]
   (fn [rf]
     (let [finds (volatile! 0)
           look (xform (fn
                         ([] nil)
                         ([_] nil)
                         ([_ item]
                          ;; this gets called only when an item would be added to the collection
                          (vswap! finds inc))))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result item]
          (vreset! finds 0)
          (look nil item)
          (if (<= min @finds max)
            result
            (rf result item))))))))

;; TODO: move to transducers lib
(defn branch [xforms]
  ;; the merge is built-in, unlike the lazy branch which is followed by a merge-exhaustive
  ;; in this context, merge-round-robin makes more sense.
  ;; so the branch doesn't actually ever produce separate streams, it just goes directly to the merged result.
  ;; is it a set of streams per-vertex or a set of streams for all vertices?
  ;; I think the latter.
  (fn [rf]
    (let [xforms (mapv
                   #(% (fn
                         ;; Don't pass the completing of the rf through because completing multiple times is invalid
                         ;; and this transducer will do that after its child xforms have been completed.
                         ([result] result)
                         ([result item] (rf result item))))
                   xforms)]
      (fn
        ([] (rf))
        ([result]
         (rf (reduce (fn [result xform] (xform result)) result xforms)))
        ([result item]
         (reduce (fn [result xform] (xform result item)) result xforms))))))

(comment
  (into [] (branch [(mapcat str)
                    (map str)
                    (comp
                      (mapcat #(range 10 %))
                      (map #(- % 5))
                      (mapcat range))])
    [12 13]))

(defn with
  "Filters the route for elements where the result of calling the function children
   (fn [e]) are equal to v. If v is a set, then check that the result of
   calling children is in the set."
  [children v]
  (if (set? v)
    (filter (fn [e] (v (children e))))
    (filter (fn [e] (= v (children e))))))

(defn is
  "Filter for items in the route equal to v."
  {:see-also ["isn't"]}
  [v]
  (filter #(= v %)))

(defn isn't
  "Filter for items in the route not equal to v."
  {:see-also ["is"]}
  [v]
  (filter #(not= v %)))

(defn one-of
  "Filter for items in the route equal to one of the items in vs."
  [vs]
  (filter (if (set? vs) vs (set vs))))

(defn none-of
  "Filter for items in the route equal to one of the items in vs."
  [vs]
  (remove (if (set? vs) vs (set vs))))


(comment
  (into [] (neg-lookahead (is 'x)) [1 2 'x 3])
  (into [] (lookahead (is 'x)) [1 2 'x 3])

  (into []
    (lookahead {:min 10 :max 20}
      (comp
        (mapcat range)
        (filter even?)))
    (range 50))
  (into []
    (neg-lookahead {:min 10 :max 20}
      (comp
        (mapcat range)
        (filter even?)))
    (range 50)))
