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
            [fermor.core :refer [ensure-seq]])
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
  (fermor.kind-graph V E-> E->in)
  (fermor.core
    linear forked graph add-edge add-edges-from
    add-edges-to get-edge add-edge! add-vertex add-vertex! remove-vertex
    remove-vertex! add-vertices! add-edges! has-vertex? get-vertex! reload
    set-document update-document update-document!
    vertices edges unwrap
    label out-edges in-edges out-edge-count in-edge-count both-edge-count
    followed-forward? followed-reverse? go-back go-on
    transpose subseq-route rsubseq-route
    emit-and-continue emit emit-and-chain emit-and-cut continue chain ignore cut)
  (xn.transducers
    counted merged
    cond-branch distinct-by lasts-by append
    lookahead neg-lookahead branch grouped-by group-count
    sorted-group-count group-by-count sorted-group-by-count
    distinct-by sorted sorted-by section-map map*))

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
                                  (if-let [edges (get-edges g (first ls) (first egs) vertex)]
                                    (Lists/concat r edges)
                                    r)
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

(defn with-paths [] (map with-path))
(defn paths [] (map path))

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

(defn both-e*
  ([] (map (fn [v] (concat (-in-edges v) (-out-edges v)))))
  ([labels] (branch (in-e* labels) (out-e* labels))))

(defn both-e
  ([] (comp (both-e*) cat))
  ([labels] (comp (both-e* labels) cat)))

(defn out-v [] (map out-vertex))
(defn in-v [] (map in-vertex))

(defn other-v [] (map go-on))
(defn same-v [] (map go-back))

(defn both-v []
  "Returns a lazy seq of vertices out of a collection of edges."
  (mapcat #(vector (in-vertex %) (out-vertex %))))

(defn mapmap [f]
  (map #(mapv f %)))

(defn in*
  ([] (comp (in-e*) (mapmap out-vertex)))
  ([labels] (comp (in-e* labels) (mapmap out-vertex))))

(defn in
  ([] (comp (in-e) (out-v)))
  ([labels] (comp (in-e labels) (out-v))))

(defn out*
  ([] (comp (out-e*) (mapmap in-vertex)))
  ([labels] (comp (out-e* labels) (mapmap in-vertex))))

(defn out
  ([] (comp (out-e) (in-v)))
  ([labels] (comp (out-e labels) (in-v))))

(defn both*
  ([] (comp (both-e*) (mapmap go-on)))
  ([labels] (comp (both-e* labels) (mapmap go-on))))

(defn both
  ([] (comp (both-e) (other-v)))
  ([labels] (comp (both-e labels) (other-v))))

(defn in-sorted [labels sort-by-f]
  (comp (in* labels) (map #(sort-by sort-by-f %)) cat))

(defn out-sorted [labels sort-by-f]
  (comp (out* labels) (map #(sort-by sort-by-f %)) cat))

(defn documents []
  (map get-document))

(defn element-ids []
  (map element-id))

(defn has-property [k v]
  (filter (fn [e] (= v (get (get-document e) k)))))

(defn make-pairs
  ([f] (map (fn [v] [v (f v)])))
  ([f0 f1] (map (fn [v] [(f0 v) (f1 v)]))))

(defn section
  ([xform]
   (tx/section xform))
  ([f xform]
   ;; NOTE: the original did mapcat on the result of f, but it's much more
   ;; flexible to use map and allow the user to add cat if needed.
   (comp
     (tx/section xform)
     (map f))))

(defn context [f xform]
  (comp
    (tx/section (branch
                  (map identity)
                  (tx/section xform)))
    (map (fn [[v section]] (f v section)))))

(defn sorted-section
  ([xform]
   (tx/section (comp xform sorted)))
  ([sort-by-f xform]
   (tx/section (comp xform (sorted-by sort-by-f)))))

#_
(into []
  (sorted-section (mapcat (constantly [10 3 2 1 9])))
  (range 3))

;; (into [] (context vector (mapcat range)) (range 10))

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
  (filter #(not (= v %))))

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

(defn degree
  ([v]
   (tx/counted (both-e) [v]))
  ([v labels]
   (tx/counted (both-e labels) [v])))

(defn in-degree
  ([v]
   (count (-in-edges v)))
  ([v labels]
   (count (-in-edges v labels))))

(defn out-degree
  ([v]
   (count (-out-edges v)))
  ([v labels]
   (count (-out-edges v labels))))


(let [partition (partition-all 32)]
  (defn chunked [xform coll]
    (letfn [(cont [[block & more]]
              (when block
                (let [n (int (count block))
                      b (chunk-buffer n)]
                  (dotimes [i n]
                    (chunk-append b (nth block i)))
                  (chunk-cons (chunk b) (lazy-seq (cont more))))))]
      (cont (eduction (comp xform partition) coll)))))


;; The graph traversal code below can not be in the form of transducers because laziness is
;; critical for this type of work. However individual steps here are transducers. Instead
;; of a function to get children, xchildren is a transducer which will be instantiated with
;; a single element collection.
;;
;; This seems to have a roughly 3x performance penalty compared with the lazy
;; seq version in rough initial testing: 50M descents in 12s vs 4s. This may
;; change/invert as the work to get children gets heavier

(defn descend
  "A power-tool for recursively traversing the graph. See also: descents, all, deepest

  The arity 3 version omits the control function. It is like the arity 4 version
  where the control function always returns :loop-and-emit.

  Arguments:

    `path`: The starting path that will be appended to as the function descends deeper into the graph.
            Should be either nil or a vector. If nil, path will not be tracked.
    `control`: A function that guides the descent. Should be a `(fn [path current])`. See below for valid return values.
    `children`: A function that produces child elements for the current element: Should be a `(fn [path current])`.
    `coll`: The starting collection. Elements in the starting collection will be passed to the control
            function and may be emitted.

  Table of Valid Control Return Values:

    ;;                      [emit   children  siblings  reset-path]
    (def emit-and-continue  [true     true     true      false])
    (def emit               [true     false    true      false])
    (def emit-and-chain     [true     true     false     false])
    (def emit-and-cut       [true     false    false     false])
    (def continue           [false    true     true      false])
    (def ignore             [false    false    true      false])
    (def chain              [false    true     false     false])
    (def cut                [false    false    false     false])

    The control signal is a vector of 4 booleans:
       0. emit: control whether the current element or path is emitted into the result setBit
       1. children: control whether to descend to the current element's children
       2. siblings: control whether to continue to traverse to the current element's siblings
       3. reset-path: if true, the path vector will be reset to [], meaning that any future emitted or control path will not have previous history in it.

   Hidden cycle protection:

     This section describes a failsafe to prevent descend from being caught
     permanently in a graph cycle that is producing no results. If you expect
     cycles, you are probably better off looking at the path that is passed to the
     control and children functions to detect a repeating pattern based on your
     traversal logic. This function will by default prevent traversing more than
     *cut-no-results* (10,000,000) levels deep while returning no matching results.
     Every *no-results-interval* (10,000) child levels, it will call the
     *no-results* (fn [chk-buffer no-result down right]) function to allow it to
     produce a resolution or to continue the search. Some standard resolution
     functions are included: descend/cut-no-results, descend/continue-no-results, and
     descend/value-for-no-results. Return their return value. You can modify the behavior
     of this system by binding the following dynamic vars:

       descend/*cut-no-results*
       descend/*no-results-interval*
       descend/*no-results*

  Handling cycles:

    Cycles that are included in the results can be handled outside descend
    because the results produced are lazy. See prevent-cycles or no-cycles!
    below."
  {:see-also ["descents" "all" "deepest" "all-paths" "deepest-paths"]}
  ([path xchildren coll]
   (lazy-seq (extrude (*descend path (fn [path item] (chunked xchildren [[path item]])) coll))))
  ([path control xchildren coll]
   (lazy-seq (extrude (*descend path control (fn [path item] (chunked xchildren [[path item]])) coll *no-result-interval* 0)))))


(defn descents
  "Descents is a variant of descend which returns the entire descent
  path as a vector rather than just the resulting element.

  Note that the descent path is not the same as using with-path to produce
  proper paths. The descent path only includes the actual elements that are
  passed into the children function in the course of operation.

  Please see `descend` for details. In descents, the initial path is not optional."
  {:see-also ["descend" "all" "deepest" "all-paths" "deepest-paths"]}
  ([path xchildren coll]
   (lazy-seq (extrude (*descents path (fn [path item] (chunked xchildren [[path item]])) coll))))
  ([path control xchildren coll]
   (lazy-seq (extrude (*descents path control (fn [path item] (chunked xchildren [[path item]])) coll *no-result-interval* 0)))))

(defn- ev-pred [f1 f2]
  (fn
    ([a]
     (and (f1 a) (f2 a)))
    ([a b]
     (and (f1 a b) (f2 a b)))))

(defn drop-path [xform]
  (fn [rf]
    (let [xform (xform rf)]
      (fn
        ([] (xform))
        ([result]
         (xform result))
        ([result [path input]]
         (xform result input))))))

(defn when-path [pred xform]
  (fn [rf]
    (let [xform (xform rf)]
      (fn
        ([] (xform))
        ([result]
         (xform result))
        ([result [path input]]
         (if (pred path input)
           (xform result [path input])
           result))))))

(defn when-path! [pred xform]
  (when-path pred (drop-path xform)))

(defn- build-all
  "Does everything just as its name implies."
  [desc* control cut-cycles? pred path-pred element-pred xchildren r]
  (let [paths (when (or cut-cycles? path-pred pred (identical? descents desc*))
                (if cut-cycles?
                  (ordered-set)
                  []))
        depth-pred (when-let [n (cond (nat-int? path-pred) path-pred (nat-int? pred) pred)]
                     (fn dpred [p] (< (count p) n)))
        path-pred (if (nat-int? path-pred) nil path-pred)
        path-pred (if (and path-pred depth-pred)
                    (ev-pred path-pred depth-pred)
                    (or path-pred depth-pred))
        ppe (cond (and (fn? path-pred) element-pred) (fn eppred [path e] (and (path-pred path) (element-pred e)))
                  (fn? path-pred)                    (fn ppred [path e] (path-pred path))
                  element-pred                       (fn epred [path e] (element-pred e)))
        pred (if cut-cycles?
               (if (fn? pred)
                 (fn cppred [p e] (and (not (p e)) (pred p e)))
                 (fn cpred [p e] (not (p e)))))
        pred (cond (and (fn? pred) ppe) (ev-pred pred ppe)
                   (fn? pred) pred
                   ppe ppe)
        desc** (if control
                 (partial desc* paths control)
                 (partial desc* paths))
        xchildren (unwrapping-path xchildren)]
    (if pred
      (desc** (when-path pred xchildren) (ensure-seq r))
      (desc** xchildren (ensure-seq r)))))

(defn all
  "Produces a lazy sequence of every element in the route and all of their
  children. Cuts cycles.

  `pred` is a `(fn [path element])` that returns true to continue iterating.

  `pred` or `path-pred` may be a natural integer, meaning the maximum path
  length allowed before iterating. Note that the internal path is only the
  elements seen by the iteration and is not the same as the more complete path
  produced by `with-path`."
  ([xchildren r]
   (build-all descend nil true nil nil nil xchildren r))
  ([pred xchildren r]
   (build-all descend nil true pred nil nil xchildren r))
  ([path-pred element-pred xchildren r]
   (build-all descend nil true nil path-pred element-pred xchildren r)))

(defn all-with-cycles
  "Produces a lazy sequence of every element in the route and all of their
  children. Does not cut cycles.

  See `all` for details on arities."
  ([children r]
   (build-all descend nil false nil nil nil children (ensure-seq r)))
  ([children pred r]
   (build-all descend nil false pred nil nil children (ensure-seq r)))
  ([children path-pred el-pred r]
   (build-all descend nil false nil path-pred el-pred children (ensure-seq r))))

(defn- deepest-control [xchildren]
  (fn [p e] (if (seq (chunked xchildren [e])) continue emit)))

(defn deepest
  "Produces a lazy sequence of every leaf node reachable by traversing all of
  the children of every element in the route. Cuts cycles.

  See `all` for details on arities."
  ([children r]
   (build-all descend (deepest-control children) true nil  nil nil children r))
  ([pred children r]
   (build-all descend (deepest-control children) true pred nil nil children r))
  ([path-pred element-pred children r]
   (build-all descend (deepest-control children) true nil path-pred element-pred children r)))


(defn all-paths
  "Produces a lazy sequence of paths to every element in the route and all of
  their children. Cuts cycles.

  See `all` for details on arities."
  ([xchildren r]
   (build-all descents nil true nil nil nil xchildren r))
  ([pred xchildren r]
   (build-all descents nil true pred nil nil xchildren r))
  ([path-pred element-pred xchildren r]
   (build-all descents nil true nil path-pred element-pred xchildren r)))

(defn all-paths-to
  "Produce a lazy sequence of all paths to every element where pred returns true.

  Once a path is returned, that path will be cut and no further searching will happen.

  If there are multiple paths to the same element where pred returns true, all
  of those paths will be returned.

  Cuts cycles"
  [pred children r]
  (descents (ordered-set)
    (fn control [path e] (if (pred path e) emit continue))
    (when-path! (fn [path e] (not (path e))) children)
    r))

(defn search
  "Produce a lazy sequence of all elements where pred returns true.

  Once an element is returned, its children will not be seached.

  If there are multiple paths to the same result, the result will be returned
  multiple times.

  Cuts cycles"
  [pred children r]
  (descend #{}
    (fn control [path e] (if (pred path e) emit continue))
    (fn [path e] (when-not (path e) (children e)))
    r))

(defn all-paths-with-cycles
  "Produces a lazy sequence of paths to every element in the route and all of
  their children. Does not cut cycles.

  See `all` for details on arities."
  ([children r]
   (build-all descents nil false nil nil nil children r))
  ([pred children r]
   (build-all descents nil false pred nil nil children r))
  ([path-pred element-pred children r]
   (build-all descents nil false nil path-pred element-pred children r)))

(defn deepest-paths
  "Produces a lazy sequence of paths to every leaf node reachable by traversing
  all of the children of every element in the route. Cuts cycles.

  See `all` for details on arities."
  ([children r]
   (build-all descents (deepest-control children) true nil nil nil children r))
  ([pred children r]
   (build-all descents (deepest-control children) true pred nil nil children r))
  ([path-pred element-pred children r]
   (build-all descents (deepest-control children) true nil path-pred element-pred children r)))

(defn- all-cycles-control [path e]
  (if (= e (first path))
    emit-and-cut
    continue))

(defn all-cycles
  "Produces a lazy sequence of elements that have a cyclic path.

  See `all` for details on arities."
  ;; force a path pred to turn on ordered-sets in build-all.
  ([children r]
   (build-all descend all-cycles-control true nil (constantly true) nil children r))
  ([pred children r]
   (build-all descend all-cycles-control true pred (constantly true) nil children r))
  ([path-pred element-pred children r]
   (build-all descend all-cycles-control true nil
              (or path-pred (constantly true)) element-pred children r)))

(defn all-cycle-paths
  "Produces a lazy sequence of cyclic paths.

  See `all` for details on arities."
  ;; force a path pred to turn on ordered-sets in build-all.
  ([children r]
   (build-all descents all-cycles-control true nil (constantly true) nil children r))
  ([pred children r]
   (build-all descents all-cycles-control true pred (constantly true) nil children r))
  ([path-pred element-pred children r]
   (build-all descents all-cycles-control true nil
              (or path-pred (constantly true)) element-pred children r)))

(defn is-cycle
  "Matches only if the current element is a member of the results from f."
  [children r]
  (lookahead #(all-cycles 1 children %) r))

(defn no-cycle
  "Matches only if the current element is not a member of the results from f."
  [children r]
  (neg-lookahead #(all-cycles 1 children %) r))
