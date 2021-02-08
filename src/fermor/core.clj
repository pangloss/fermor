(ns fermor.core
  (:require [conditions :refer [condition manage lazy-conditions error default]]
            [potemkin :refer [import-vars]]
            [fermor.protocols :refer [-out-edges -in-edges traversed-forward -label -unwrap]]
            fermor.graph
            fermor.kind-graph
            fermor.path)
  (:import clojure.lang.IMeta
           (fermor.protocols TraversalDirection Wrappable)))

(import-vars (fermor.protocols set-config!
                               ;; Predicates
                               graph? vertex? edge? element?
                               ;; Graph
                               graph get-vertex all-vertices
                               ;; MutableGraph
                               add-vertices add-vertex add-edge add-edges set-property
                               ;; Element
                               element-id get-property
                               ;; Edge
                               out-vertex in-vertex
                               ;; Path
                               reverse-path
                               ;; KindId
                               id k lookup)
             ;; Bifurcan Graph
             (fermor.graph linear forked dag-edge digraph-edge undirected-edge build-graph
                           vertices-with-edge
                           ;; read printed graph elements
                           v e-> e<-)
             ;; Path
             (fermor.path with-path path? path no-path no-path! cyclic-path?)
             ;; Kind Graph
             (fermor.kind-graph V E-> E<-))

(defn ensure-seq
  "Returns either nil or something sequential."
  [x]
  (if (or (nil? x) (sequential? x))
    x
    [x]))

(defn unwrap
  "Recursively unwrap any element or just return the input if it's not wrapped"
  [e]
  (if (satisfies? Wrappable e)
    (-unwrap e)
    e))

(defn label
  "This edge label"
  [e]
  (-label e))

(defn out-edges
  "Edges pointing out of this vertex"
  ([v] (-out-edges v))
  ([v labels] (-out-edges v labels)))

(defn in-edges
  "Edges pointing in to this vertex"
  ([v] (-in-edges v))
  ([v labels] (-in-edges v labels)))

;; TraversalDirection methods

(defn ->?
  "Returns true if we followed an out-edge to get to this edge."
  [e]
  (if (satisfies? fermor.protocols/TraversalDirection e)
    (traversed-forward e)
    (condition :traversal-direction/unknown e (default true))))

(defn <-?
  "Returns true if we followed an in-edge to get to this edge."
  [e]
  (not (->? e)))

(defn go-back
  "Returns the vertex we used to get to this edge"
  {:see-also ["->?" "go-on" "same-v"]}
  [e]
  (if (->? e) (out-vertex e) (in-vertex e)))

(defn go-on
  "Returns the vertex we did not use to get to this edge"
  {:see-also ["->?" "go-back" "other-v"]}
  [e]
  (if (->? e) (in-vertex e) (out-vertex e)))

(defn- fast-traversal
  "Requires that all vertices are from the same graph to work."
  ([traversal labels r]
   (lazy-seq
    (let [r (ensure-seq r)]
      ;; (let [labels (-prepare-labels (route-graph r) labels)]
      (map #(traversal % nil labels) r))))
  ([f traversal labels r]
   (lazy-seq
    (let [r (ensure-seq r)]
      ;; (let [labels (-prepare-labels (route-graph r) labels)]
      (map #(f (traversal % nil labels)) r)))))

(defn in-e*
  "Returns a lazy seq of lazy seqs of edges.

  Each entry represents the edges for a single vertex. If a vertex has no edges, its empty seq will still be included.

  If f is given, it is executed against the edges for each vertex."
  ([r]
   (cond
     (vertex? r) [(-in-edges r)]
     (nil? r) nil
     :else (map -in-edges r)))
  ([labels r]
   (cond
     (vertex? r) [(-in-edges r (ensure-seq labels))]
     (nil? r) nil
     :else (fast-traversal -in-edges (ensure-seq labels) r)))
  ([f labels r]
   (cond
     (vertex? r) [(f (-in-edges r (ensure-seq labels)))]
     (nil? r) nil
     :else (fast-traversal f -in-edges (ensure-seq labels) r))))

(defn in-e
  "Returns a lazy seq of edges.

  If f is given, it is executed once for the edges of each vertex"
  ([r] (apply concat (in-e* r)))
  ([labels r]
   (apply concat (in-e* labels r)))
  ([f labels r]
   (apply concat (in-e* f labels r))))

(defn out-e*
  "Returns a lazy seq of lazy seqs of edges.

  Each entry represents the edges for a single vertex. If a vertex has no edges, its empty seq will still be included.

  If f is given, it is executed against the edges for each vertex."
  ([r]
   (cond
     (vertex? r) [(-out-edges r)]
     (nil? r) nil
     :else (map -out-edges r)))
  ([labels r]
   (cond
     (vertex? r) [(-out-edges r (ensure-seq labels))]
     (nil? r) nil
     :else (fast-traversal -out-edges (ensure-seq labels) r)))
  ([f labels r]
   (cond
     (vertex? r) [(f (-out-edges r (ensure-seq labels)))]
     (nil? r) nil
     :else (fast-traversal f -out-edges (ensure-seq labels) r))))

(defn out-e
  "Returns a lazy seq of edges.

  If f is given, it is executed once for the edges of each vertex"
  ([r] (apply concat (out-e* r)))
  ([labels r]
   (apply concat (out-e* labels r)))
  ([f labels r]
   (apply concat (out-e* f labels r))))

(defn both-e*
  "Returns a lazy seq of lazy seqs of edges.

  Each entry represents the edges for a single vertex. If a vertex has no edges, its empty seq will still be included.

  If f is given, it is executed against the edges for each vertex."
  ([r]
   (cond
     (vertex? r) [(concat (-in-edges r) (-out-edges r))]
     (nil? r) nil
     :else
     (map (fn [v] (concat (-in-edges v) (-out-edges v)))
          r)))
  ([labels r]
   (cond
     (vertex? r)
     (let [labels (ensure-seq labels)]
       [(concat (-in-edges r labels) (-out-edges r labels))])
     (nil? r) nil
     :else
     (fast-traversal (fn [v _ l] (concat (-in-edges v nil l) (-out-edges v nil l)))
                     (ensure-seq labels)
                     r)))
  ([f labels r]
   (let [labels (ensure-seq labels)]
     (cond
       (vertex? r) [(f (concat (-in-edges r labels) (-out-edges r labels)))]
       (nil? r) nil
       :else
       (fast-traversal f
                       (fn [v _ l] (concat (-in-edges v nil l) (-out-edges v nil l)))
                       labels
                       r)))))

(defn both-e
  "Returns a lazy seq of edges.

  If f is given, it is executed once for the edges of each vertex"
  ([r] (apply concat (both-e* (ensure-seq r))))
  ([labels r]
   (apply concat (both-e* labels (ensure-seq r))))
  ([f labels r]
   (apply concat (both-e* f labels (ensure-seq r)))))

(defn out-v
  "Returns a lazy seq of vertices out of a collection of edges."
  [r]
  (cond
    (edge? r) [(out-vertex r)]
    (nil? r) nil
    :else (map out-vertex (ensure-seq r))))

(defn in-v
  "Returns a lazy seq of vertices in to a collection of edges."
  [r]
  (cond
    (edge? r) [(in-vertex r)]
    (nil? r) nil
    :else (map in-vertex (ensure-seq r))))

(defn other-v
  "Returns a lazy seq of vertices on the other side of the edge that we came from."
  [r]
  (map go-on r))

(defn same-v
  "Returns a lazy seq of vertices on the same side of the edge that we came from."
  [r]
  (map go-back r))

(defn both-v
  "Returns a lazy seq of vertices out of a collection of edges."
  [r]
  (cond
    (edge? r) [(in-vertex r) (out-vertex r)]
    (nil? r) nil
    :else (mapcat #(vector (in-vertex %) (out-vertex %)) (ensure-seq r))))

(defn in*
  "Returns a lazy seq of lazy seqs of vertices with edges pointing in to this vertex.

  If f is given, it is called once for each collection of vertices related to a single vertex in the route."
  ([r]
   (->> r in-e* (map out-v)))
  ([labels r]
   (in-e* out-v labels r))
  ([labels f r]
   (in-e* (comp f out-v) labels r)))

(defn out*
  "Returns a lazy seq of lazy seqs of vertices with edges pointing out of this vertex.

  If f is given, it is called once for each collection of vertices related to a single vertex in the route."
  ([r]
   (->> r out-e* (map in-v)))
  ([labels r]
   (out-e* in-v labels r))
  ([labels f r]
   (out-e* (comp f in-v) labels r)))

(defn both*
  "Returns a lazy seq of lazy seqs of vertices with edges pointing both in and out of this vertex "
  ([r] (->> r both-e* (map other-v)))
  ([labels r] (both-e* go-on labels r))
  ([labels f r] (both-e* (comp f go-on) labels r)))

(defn both
  "Returns a lazy seq of vertices with edges pointing both in and out of this vertex "
  ([r] (apply concat (both* r)))
  ([labels r] (apply concat (both* labels r)))
  ([labels f r] (apply concat (both* labels f r))))

(defn in
  "Returns a lazy seq of vertices with edges pointing in to this vertex "
  ([r] (apply concat (in* r)))
  ([labels r] (apply concat (in* labels r)))
  ([labels f r] (apply concat (in* labels f r))))

(defn out
  "Returns a lazy seq of vertices with edges pointing out of this vertex "
  ([r] (apply concat (out* r)))
  ([labels r] (apply concat (out* labels r)))
  ([labels f r] (apply concat (out* labels f r))))

(defn in-sorted
  "Like in, but use sort-by-f to sort the elements attached to each vertex before including them in the overall collection."
  [labels sort-by-f r]
  (in labels #(sort-by sort-by-f %) r))

(defn out-sorted
  "Like out, but use sort-by-f to sort the elements attached to each vertex before including them in the overall collection."
  [labels sort-by-f r]
  (out labels #(sort-by sort-by-f %) r))

(defn properties
  "Return the property from each element"
  [r]
  (map get-property r))

(defn has-property
  "The property must be indexable, and if it is, returns true if the property
  contains the given key value pair."
  [r k v]
  (filter (fn [e] (= v (get (get-property e) k))) r))

;; for sorted sets:

(defn subseq-route
  "Like subseq except the sorted set is the last argument"
  ([test key r]
   (assert (instance? clojure.lang.Sorted r))
   (subseq r test key))
  ([start-test start-key end-test end-key r]
   (assert (instance? clojure.lang.Sorted r))
   (subseq r start-test start-key end-test end-key)))

(defn rsubseq-route
  "Like rsubseq except the sorted set is the last argument"
  ([test key r]
   (assert (instance? clojure.lang.Sorted r))
   (rsubseq r test key))
  ([start-test start-key end-test end-key r]
   (assert (instance? clojure.lang.Sorted r))
   (rsubseq r start-test start-key end-test end-key)))

;;

;; (defn- route-graph [r]
;;   (or (:graph (meta r)) #_(get-graph (first r))))

(defn fast-sort-by
  "Works like sort-by but creates an intermediate collection so that f is only called once per element.

   Much faster if f has any cost at all."
  [f coll]
  (->> coll
       (map (juxt f identity))
       (sort-by #(nth % 0))
       (map #(nth % 1))))

(defn group-siblings
  "For efficiently traversing through relationships like
     (source)-[has-parent]->(parent)<-[has-parent]-(dest)
   even if the parent has multiple children.

   Returns a lazy seq of lazy seqs of siblings.

   get-siblings is a function that given source returns dest.
   to-parent and from-parent are functions that when combined will traverse from source to dest."
  ([get-siblings r]
   (letfn [(sibling-seq [[v & [vs]]]
             (lazy-seq
               (cons (->> v
                          get-siblings
                          (filter #(not= v %)))
                     (sibling-seq vs))))]
     (sibling-seq r)))
  ([to-parent from-parent r]
   (group-siblings (comp from-parent to-parent) r)))

(declare join)

(defn siblings
  "For efficiently traversing through relationships with the same edge direction and label in relation to a parent node
     (source)-[has-parent]->(parent)<-[has-parent]-(dest)
   even if the parent has multiple members.

   get-siblings is a function that given source returns dest.
   to-parent and from-parent are functions that when combined will traverse from source to dest."
  ([get-siblings r]
   (join (group-siblings get-siblings r)))
  ([to-parent from-parent r]
   (join (group-siblings to-parent from-parent r))))

(defn make-pairs
  "Map each element in r to a pair of [element (f element)]."
  ([f r]
   (map (fn [v] [v (f v)]) r))
  ([f0 f1 r]
   (map (fn [v] [(f0 v) (f1 v)]) r)))

(defn section
  "Apply a the section route to an element and then apply f to that section of the results.

  Both f and section are functions."
  [f section r]
  (mapcat (comp f section) r))

(defn context
  "Like section, but the function f receives the element as context together with the section route."
  [f section r]
  (mapcat (fn [e] (f e (section e))) r))

(defn sorted-section
  "This is mostly just an example of how to use sections to do sorting."
  [sort-by-f section r]
  (mapcat (comp #(fast-sort-by sort-by-f %) section) r))

(defn gather
  "Collect all results into a vector containing one collection."
  ([r] (gather [] r))
  ([coll r] [(into coll r)]))

(defn spread
  "Turn a collection of collections back into a single lazy stream. See also: merge-round-robin."
  [r]
  (apply concat r))

(defn join
  "Turn a collection of collections back into a single lazy stream. See also: merge-round-robin."
  [r]
  (apply concat r))

(defn lookahead
  "Ensure that the function produces a collection of at least one item.

   Use the arity-2 version to specify that there must be at least min and/or at
   most max items in the route. If min or max is nil that limit will not be
   enforced."
  ([f r]
   (filter (comp seq f) (ensure-seq r)))
  ([{:keys [min max]} f r]
   (cond
     (and min max)
     (filter #(<= min (count (take (inc max) (f %))) max)
             (ensure-seq r))
     min
     (filter #(= min (count (take min (f %))))
             (ensure-seq r))
     max
     (filter #(<= (count (take (inc max) (f %))) max)
             (ensure-seq r))
     :else
     r)))

(defn lookahead-element
  "This version of lookahead takes an individual element as source rather than a route.

   Ensure that the function produces a collection of at least one item.

   Use the arity-2 version to specify that there must be at least min and/or at
   most max items in the route. If min or max is nil that limit will not be
   enforced."
  ([f e]
   (when (seq (f e)) e))
  ([{:keys [min max]} f e]
   (cond
     (and min max)
     (when (<= min (count (take (inc max) (f e))) max)
       e)
     min
     (when (= min (count (take min (f e))))
       e)
     max
     (when (<= (count (take (inc max) (f e))) max)
       e)
     :else
     e)))

(defn neg-lookahead
  "Ensure that the function does NOT produce a collection of at least one item.

   Use the arity-2 version to specify that there must NOT be at least min
   and/or at most max items in the route. If min or max is nil that limit will
   not be enforced. The arity-2 version of neg-lookahead is not really recommended
   as it is a little bit confusing."
  ([f r]
   (filter #(not (seq (f %))) (ensure-seq r)))
  ([{:keys [min max]} f r]
   (cond
     (and min max)
     (filter #(not (<= min (count (take (inc max) (f %))) max))
             (ensure-seq r))
     min
     (filter #(not (= min (count (take min (f %)))))
             (ensure-seq r))
     max
     (filter #(not (<= (count (take (inc max) (f %))) max))
             (ensure-seq r))
     :else
     r)))

(defn branch
  "Create a collection of lazy sequences, one for each function in the provided collection fs.

   Typically used together with either merge-round-robin or merge-exhaustive.

   Arguments:

    fs: a collection of functions (fn [r]), each returning a collection or nil. Each will be called with the same starting route."
  [fs r]
  (mapv (fn [f] (f r)) fs))

(defn keyed-branch [pairs r]
  (reduce (fn [m [k f]] (assoc m k (f r))) {} (partition 2 pairs)))

(defn merge-exhaustive
  "Merge a set of sequnces (or branches), including the full contents of each branch in order from first to last."
  [r]
  (if (map? r)
    (apply concat (vals r))
    (apply concat r)))

(defn merge-round-robin
  "Merge a set of sequences (or branches), taking one chunk from each sequence in turn until all sequences are exhausted.

   rs must be a vector."
  [rs]
  (if (map? rs)
    (merge-round-robin (vals rs))
    (let [rs (vec rs)]
      (lazy-seq
       (let [r (seq (first rs))
             rs (subvec rs 1)]
         (if (seq rs)
           (if (chunked-seq? r)
             (chunk-cons (chunk-first r)
                         (let [r (chunk-next r)]
                           (if r
                             (merge-round-robin (conj rs r))
                             (merge-round-robin rs))))
             (let [b (chunk-buffer 32)
                   r (loop [i 0 [x & r] r]
                       (chunk-append b x)
                       (cond (or (= 32 i) (chunked-seq? (seq r))) r
                             r (recur (inc i) r)))]
               (chunk-cons (chunk b)
                           (if r
                             (merge-round-robin (conj rs r))
                             (merge-round-robin rs)))))
           r))))))

(deftype Cons [first tail])
(deftype Concat [head tail])
(defrecord NoResult [path ^:long depth])

(defmethod print-method Cons [^Cons v ^java.io.Writer w]
  (.write w "(Cons ")
  (print-method (.first v) w)
  (.write w ", ")
  (if (.tail v)
    (.write w (str (class (.tail v))))
    (.write w "nil"))
  (.write w ")"))

(defmethod print-method Concat [^Concat v ^java.io.Writer w]
  (.write w "(Concat ")
  (if (.head v)
    (.write w (str (class (.head v))))
    (.write w "nil"))
  (.write w ", ")
  (if (.tail v)
    (.write w (str (class (.tail v))))
    (.write w "nil"))
  (.write w ")"))

;;                     [emit children siblings reset-path]
(def emit-and-continue  [true   true   true   false])
(def emit               [true   false  true   false])
(def emit-and-chain     [true   true   false  false])
(def emit-and-cut       [true   false  false  false])
(def continue           [false  true   true   false])
(def chain              [false  true   false  false])
(def ignore             [false  false  true   false])
(def cut                [false  false  false  false])

(defn reset-path [instruction]
  (update instruction 3 true))

;; Must be used together with extrude to work around limitation in Clojure lazy-seq concat
(defn- *descend
  ([path f coll]
   (when-let [[e & more] (seq coll)]
     #(Cons. e
             (Concat. (*descend (when path (conj path e)) f (ensure-seq (f path e)))
                      (when more (*descend path f more))))))
  ([path control f coll ^:long nri ^:long recur-depth] ; nri is no-results-interval
   (when-let [[e & more] (seq coll)]
     (let [[emit children siblings reset-path] (control path e)
           results (when children
                     (ensure-seq (f path e)))
           path (when-not (nil? path) (if reset-path [] path))]

       (case [(boolean emit) (boolean children) (boolean siblings)]

         [true true true] ; (:emit-and-loop true :loop-and-emit)
         #(Cons. e (Concat. (*descend (when path (conj path e)) control f results nri 0)
                            (*descend path control f more nri 0)))

         [true false true] ; :emit
         #(Cons. e (*descend path control f more nri 0))

         [true true false] ; (:emit-and-chain :chain-and-emit)
         #(Cons. e (*descend (when path (conj path e)) control f results nri 0))

         [true false false] ; (:emit-and-cut :cut-and-emit)
         #(Cons. e nil)

         [false true true] ; :loop
         (let [more #(Concat. (*descend (when path (conj path e)) control f results nri (inc recur-depth))
                              (*descend path control f more nri (inc recur-depth)))]
           (if (= nri (mod recur-depth (inc nri)))
             #(Cons. (NoResult. path recur-depth) more)
             more))

         [false true false] ; :chain
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descend (when path (conj path e)) control f results nri (inc recur-depth)))
           (recur (when path (conj path e)) control f results nri (inc recur-depth)))

         [false false true] ; (:ignore false nil)
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descend path control f more nri (inc recur-depth)))
           (recur path control f more nri (inc recur-depth)))

         [false false false]; :cut
         nil)))))

;; Must be used together with extrude to work around limitation in Clojure lazy-seq concat
(defn- *descents
  ([path f coll]
   (when-let [[e & more] (seq coll)]
     #(let [e-path (conj path e)]
        (Cons. e-path
               (Concat. (*descents e-path f (ensure-seq (f path e)))
                        (when more (*descents path f more)))))))
  ([path control f coll ^:long nri ^:long recur-depth] ; nri is no-results-interval
   (when-let [[e & more] (seq coll)]
     (let [[emit children siblings reset-path] (control path e)
           results (when children
                     (ensure-seq (f path e)))
           path (if reset-path [] path)
           e-path (conj path e)]

       (case [(boolean emit) (boolean children) (boolean siblings)]

         [true true true] ; (:emit-and-loop true :loop-and-emit)
         #(Cons. e-path (Concat. (*descents e-path control f results nri 0)
                                 (*descents path control f more nri 0)))

         [true false true] ; :emit
         #(Cons. e-path (*descents path control f more nri 0))

         [true true false] ; (:emit-and-chain :chain-and-emit)
         #(Cons. e-path (*descents e-path control f results nri 0))

         [true false false] ; (:emit-and-cut :cut-and-emit)
         #(Cons. e-path nil)

         [false true true] ; :loop
         (let [more #(Concat. (*descents e-path control f results nri (inc recur-depth))
                              (*descents path control f more nri (inc recur-depth)))]
           (if (= nri (mod recur-depth (inc nri)))
             #(Cons. (NoResult. path recur-depth) more)
             more))

         [false true false] ; :chain
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descents e-path control f results nri (inc recur-depth)))
           (recur e-path control f results nri (inc recur-depth)))

         [false false true] ; (:ignore false nil)
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descents path control f more nri (inc recur-depth)))
           (recur path control f more nri (inc recur-depth)))

         [false false false] ; :cut
         nil)))))


(defn cut-no-results
  "A possible resolution to be used by a custom *no-results* function. Stop searching the current element and its siblings."
  []
  nil)

(defn continue-no-results
  "A possible resolution to be used by a custom *no-results* function. For
   instance to continue searching siblings but not children, call
   (continue-no-results nil right), or to search children but not siblings, call
   (continue-no-results down nil)."
  [down right]
  (Concat. down right))

(defn value-for-no-results
  "A possible resolution for *no-results*. Insert a constant value into the
   result stream and then continue searching."
  [chk-buffer value down right]
  (chunk-append chk-buffer value)
  (Concat. down right))

(defn ->no-results
  "This function is used to capture the value of *cut-no-results* so that it is
   not lost when the seq it is used in is returned from the binding context."
  [^:long cut-no-results]
  (fn [chk-buffer no-result down right]
    (if (< (:depth no-result) cut-no-results)
      (continue-no-results down right)
      (continue-no-results nil right))))

(defn- *extrude [b x fno-results tails]
  ; b is always created in the calling method. It is mutable and does not need to be returned.
  (let [x (if (fn? x) (x) x)]
    (if x
      (cond (instance? Cons x)
            (let [^Cons c x]
              (let [v (.first c)]
                (if (instance? NoResult v)
                  (fno-results b v (.tail c) tails)
                  (do (chunk-append b (.first c))
                      (if (= 32 (count b))
                        (if tails
                          (Concat. (.tail c) tails)
                          (.tail c))
                        (recur b (.tail c) fno-results tails))))))
            (instance? Concat x)
            (let [^Concat c x]
              (let [head (.head c)
                    tail (.tail c)]
                (cond head
                      (recur b head fno-results (if tail
                                                  (if tails
                                                    (Concat. tail tails)
                                                    tail)
                                                  tails))
                      tail
                      (recur b tail fno-results tails)
                      :else
                      tails)))
            :else
            tails)
      tails)))

(defonce ^:dynamic ^:long *cut-no-results* 10000000)
(defonce ^:dynamic ^:long *no-result-interval* 10000)
(defonce ^:dynamic *no-results* nil)

(defn- extrude
  ([x]
   (extrude x (or *no-results* (->no-results *cut-no-results*))))
  ([x no-results]
   (lazy-seq
    (let [b (chunk-buffer 32)
          conc (*extrude b x no-results nil)]
      (if conc
        (chunk-cons (chunk b) (extrude conc no-results))
        (chunk-cons (chunk b) nil))))))


(defn descend
  "A power-tool for recursively traversing the graph. See also: descents, all, deepest

  The arity 3 version omits the control function. It is like the arity 4 version where the control function
  always returns :loop-and-emit.

  Arguments:

    path: The starting path that will be appended to as the function descends deeper into the graph.
          Should be either nil or a vector. If nil, path will not be tracked.
    control: A function that guides the descent. Should be a (fn [path current]). See below for valid return values.
    children: A function that produces child elements for the current element: Should be a (fn [path current]).
    coll: The starting collection. Elements in the starting collection will be passed to the control
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
       0) emit: control whether the current element or path is emitted into the result setBit
       1) children: control whether to descend to the current element's children
       3) siblings: control whether to continue to traverse to the current element's siblings
       4) reset-path: if true, the path vector will be reset to [], meaning that any future emitted or control path will not have previous history in it.

   Hidden cycle protection:

     This section describes a failsafe to prevent descend from being caught permanently in a graph cycle that is producing no results. If you expect
     cycles, you are probably better off looking at the path that is passed to the control and children functions to detect
     a repeating pattern based on your traversal logic. This function will by default prevent traversing more than
     *cut-no-results* (10,000,000) levels deep while returning no matching results. Every *no-results-interval* (10,000)
     child levels, it will call the *no-results* (fn [chk-buffer no-result down right]) function to allow it to
     produce a resolution or to continue the search. Some standard resolution functions are included:
     cut-no-results, continue-no-results, and value-for-no-results. Return their return value. You can modify the
     behavior of this system by binding the following dynamic vars:

       *cut-no-results*
       *no-results-interval*
       *no-results*

  Handling cycles:

    Cycles that are included in the results can be handled outside descend because the results produced are lazy. See
    prevent-cycles or no-cycles! below."
  ([path children coll]
   (lazy-seq (extrude (*descend path children coll))))
  ([path control children coll]
   (lazy-seq (extrude (*descend path control children coll *no-result-interval* 0)))))

(defn descents
  "Descents is a variant of descend which returns the path that entire descent
  path as a vector rather than just the resulting element.

   Please see descend for details. In descents, the initial path is not optional and must be a vector.

   See also: all-paths, deepest-paths"
  ([path children coll]
   (lazy-seq (extrude (*descents path children coll))))
  ([path control children coll]
   (lazy-seq (extrude (*descents path control children coll *no-result-interval* 0)))))

(defn all
  "Produces a lazy sequence of every element in the route and all of their children."
  [f r]
  (descend nil #(f %2) (ensure-seq r)))

(defn deepest
  "Produces a lazy sequence of every leaf node reachable by traversing all of the children of every element in the route."
  [f r]
  (descend nil
           (fn [p e] (if (seq (f e)) continue emit))
           #(f %2)
           (ensure-seq r)))

(defn all-paths
  "Produces a lazy sequence of paths to every element in the route and all of their children."
  [f r]
  (descents [] #(f %2) (ensure-seq r)))

(defn deepest-paths
  "Produces a lazy sequence of paths to every leaf node reachable by traversing all of the children of every element in the route."
  [f r]
  (descents []
            (fn [p e] (if (seq (f e)) continue emit))
            #(f %2)
            (ensure-seq r)))

(defn with
  "Filters the route for elements where the result of calling the function f
   (fn [v]) are equal to v. If v is a set, then check that the result of
   calling f is in the set."
  [f v r]
  (if (set? v)
    (filter (fn [e] (v (f e)))
            (ensure-seq r))
    (filter (fn [e] (= v (f e)))
            (ensure-seq r))))

(defn is
  "Filter for items in the route equal to v."
  [v r]
  (filter #{v} (ensure-seq r)))

(defn one-of
  "Filter for items in the route equal to one of the items in vs."
  [vs r]
  (filter (if (set? vs) vs (set vs)) r))

(defn distinct-in
  "Use if distinct is needed within a loop or a lookahead, or if distinctness needs to
   be coordinated across multiple places in a route.

    (let [seen (atom #{})]
      (->> r (distinct-in seen) another-r (distinct-in seen)))"
  ([seen-atom r]
   (distinct-in {:update true} seen-atom r))
  ([{:keys [update] :or {update true}} seen-atom r]
   {:pre [(instance? clojure.lang.Atom seen-atom)]}
   (let [step (fn step [xs]
                (lazy-seq
                 ((fn [[f :as xs]]
                    (when-let [s (seq xs)]
                      (if (contains? @seen-atom f)
                        (recur (rest s))
                        (do (when update (swap! seen-atom conj f))
                            (cons f (step (rest s)))))))
                  xs)))]
     (step r))))

(defn no-cycles!
  "Like prevent-cycles, but raise an :on-cycle condition (which by default
  raises an ex-info) if a cycle is encountered.

  Return false from :on-cycle to break out of the cycle, return true to continue
  cycling. If you don't handle the :on-cycle condition, an exception will be
  raised."
  [r]
  (let [seen (atom #{})]
    (lazy-conditions
     (take-while (fn [x]
                   ;; (or (vertex? x)
                   ;;     ...
                   (if (@seen x)
                     (condition :on-cycle x (error "Cycle encountered" {:vertex x}))
                     (do (swap! seen conj x) true)))
                 r))))

(defn prevent-cycles
  "Takes from the route while there is no duplicate within it. This is good for
  preventing cycles in chains of to-one or from-one relationships."
  [r]
  (manage [:on-cycle false]
    (no-cycles! r)))

(declare take-drop)

(defn drop-take
  "Alternatively drop and take chunks of the given size from the collection.

   Example:
      (drop-take [1 2 3 4] (range))
      => (1 2 6 7 8 9)"
  [steps coll]
  (when (seq steps)
    (lazy-seq
     (take-drop (rest steps) (drop (first steps) coll)))))

(defn take-drop
  "Alternatively take and drop chunks of the given size from the collection.

   Example:
      (take-drop [1 2 3 4] (range))
      => (0 3 4 5)"
  [steps coll]
  (when (seq steps)
    (lazy-seq
     (concat (take (first steps) coll)
             (drop-take (rest steps) (drop (first steps) coll))))))

(defmacro f->>
  "Returns a function wrapping the chained methods."
  [& forms]
  `(fn [r#]
     (->> r# ensure-seq ~@forms)))

(defmacro ->< [& forms-then-data]
  (let [forms (butlast forms-then-data)
        data (last forms-then-data)]
    `(-> ~data ~@forms)))

(defn pluck [f coll]
  (first (filter f coll)))

(defn index-by
  "Return an index of unique items.

  `->key` is a fn that returns a key for each item.
  `->val` is a fn that returns the value to be indexed by the returned key."
  ([->key coll]
   (persistent!
    (reduce (fn [idx x] (assoc! idx (->key x) x))
            (transient {}) coll)))
  ([->key ->val coll]
   (persistent!
    (reduce (fn [idx x] (assoc! idx (->key x) (->val x)))
            (transient {}) coll))))

(defn index-by-multi
  "Return an index of unique items.

  `->keys` is a fn that returns a vector of keys for each item. The item will be keyed to each returned key individually.
  `->val` is a fn that returns the value to be indexed by the returned keys."
  ([->keys coll]
   (persistent!
    (reduce (fn [idx x] (reduce (fn [idx k] (assoc! idx k x))
                                idx
                                (->keys x)))
            (transient {}) coll)))
  ([->keys ->val coll]
   (persistent!
    (reduce (fn [idx x]
              (let [v (->val x)]
                (reduce (fn [idx k] (assoc! idx k v))
                        idx
                        (->keys x))))
            (transient {})
            coll))))

(defn group-count
  "Return a map of {item count-equal-items} or {(f item) count-equal}"
  ([coll]
   (persistent!
    (reduce (fn [r item]
              (assoc! r item (inc (get r item 0))))
            (transient {})
            coll)))
  ([f coll]
   (persistent!
    (reduce (fn [r item]
              (let [k (f item)]
                (assoc! r k (inc (get r k 0)))))
            (transient {})
            coll))))

(defn sorted-group-count
  "Return a map of {item count-equal-items} or {(f item) count-equal}"
  ([coll]
   (reduce (fn [r item]
             (assoc r item (inc (get r item 0))))
           (sorted-map)
           coll))
  ([f coll]
   (reduce (fn [r item]
             (let [k (f item)]
               (assoc r k (inc (get r k 0)))))
           (sorted-map)
           coll)))

(defn group-by-count
  "Return a map of {count [all keys with that unique count]}"
  ([coll]
   (persistent!
    (reduce (fn [r [k count]]
              (assoc! r count (conj (get r count []) k)))
            (transient {})
            (group-count coll))))
  ([f coll]
   (persistent!
    (reduce (fn [r [k count]]
              (assoc! r count (conj (get r count []) k)))
            (transient {})
            (group-count f coll)))))

(defn group-by-count>1
  "Return a map of {count [all keys with that unique count]} where count > 1"
  ([coll]
   (persistent!
    (reduce (fn [r [k count]]
              (if (= 1 count)
                r
                (assoc! r count (conj (get r count []) k))))
            (transient {})
            (group-count coll))))
  ([f coll]
   (persistent!
    (reduce (fn [r [k count]]
              (if (= 1 count)
                r
                (assoc! r count (conj (get r count []) k))))
            (transient {})
            (group-count f coll)))))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  {:adapted-from 'clojure.core/distinct}
  ([key coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                 ((fn [[f :as xs] seen]
                    (when-let [s (seq xs)]
                      (let [val (key f)]
                        (if (contains? seen val)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen val)))))))
                  xs seen)))]
     (step coll #{}))))
