(ns fermor.core
  (:require [pure-conditioning :refer [condition manage lazy-conditions error default]]
            [potemkin :refer [import-vars import-def]]
            [flatland.ordered.set :refer [ordered-set]]
            [fermor.protocols :as proto :refer [wrappable? Wrappable -out-edges -in-edges
                                                -out-edge-count -in-edge-count
                                                to-forked to-linear
                                                traversed-forward -label -unwrap
                                                -out-edges-prepared -in-edges-prepared
                                                -out-edges-prepared2 -in-edges-prepared2
                                                -transpose -has-vertex? -get-edge]]
            [fermor.descend :refer [*descend *descents extrude *no-result-interval*]]
            fermor.graph
            [fermor.kind-graph :refer [->KGraph]]
            fermor.path)
  (:import clojure.lang.IMeta
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

;; Provide aliases of element-id since I always forget it's not vertex-id, etc.
(import-def fermor.protocols/element-id vertex-id)
(import-def fermor.protocols/element-id edge-id)


(defn linear
  "Make the graph mutable (but only therefore useable in linear code)"
  [x]
  (to-linear x))


(defn forked
  "Make the graph immutable."
  [x]
  (to-forked x))

(defmacro assert-linear! [graph]
  (if *assert*
    `(let [graph# ~graph]
       (assert (linear? graph#))
       graph#)
    graph))

(defn graph
  "Return the graph associated with the given element. If x is a graph, return
  x. If called with no argument, returns a new graph."
  ([]
   (build-graph))
  ([x]
   (proto/graph x)))

(defn add-edge
  "Add an edge from v to in-v with the given label string.

  Returns the updated graph"
  ([label out-v in-v]
   (add-edges (get-graph out-v) label [[(element-id out-v) (element-id in-v)]]))
  ([graph label out-v in-v]
   (add-edges graph label [[(element-id out-v) (element-id in-v)]]))
  ([graph label out-v in-v document]
   (add-edges graph label [[(element-id out-v) (element-id in-v) document]])))

(defn add-edges-from
  ([label from vs]
   (let [from-id (element-id from)]
     (add-edges (get-graph from) label
       (map (fn [v]
              [from-id (element-id v)])
         vs))))
  ([label from vs edge-documents]
   (let [from-id (element-id from)
         triples (map (fn [v doc]
                        [from-id (element-id v) doc])
                   vs
                   edge-documents)
         triples (if (= (count triples) (count vs))
                   triples
                   (condition :missing-documents
                     {:help "Return :triples to ignore, return other triples, or blow up"
                      :triples triples
                      :vs vs
                      :edge-documents edge-documents
                      :label label}))]
       (add-edges (get-graph from) label
         triples))))

(defn add-edges-to
  ([label vs to]
   (let [to-id (element-id to)]
     (add-edges (get-graph to) label
       (map (fn [v]
              [(element-id v) to-id])
         vs))))
  ([label vs to edge-documents]
   (let [to-id (element-id to)
         triples (map (fn [v doc]
                        [(element-id v) to-id doc])
                   vs
                   edge-documents)
         triples (if (= (count triples) (count vs))
                   triples
                   (condition :missing-documents
                     {:help "Return :triples to ignore, return other triples, or blow up"
                      :triples triples
                      :vs vs
                      :edge-documents edge-documents
                      :label label}))]
     (add-edges (get-graph to) label
       triples))))

(defn get-edge
  "Efficiently look up an edge by label and the vertices it connects.

  Returns nil if not found."
  ([label out-v in-v]
   (-get-edge (get-graph out-v) label (element-id out-v) (element-id in-v)))
  ([graph label out-v in-v]
   (-get-edge graph label (element-id out-v) (element-id in-v))))

(defn add-edge!
  "The same as add-edge, but returns the created edge instead of returning the
  graph.

  Only works on linear graphs."
  ([label out-v in-v]
   (add-edge! (get-graph out-v) label out-v in-v))
  ([graph label out-v in-v]
   (-> graph
     (or (get-graph out-v))
     assert-linear!
     (add-edge label out-v in-v)
     (get-edge
       label out-v in-v)))
  ([graph label out-v in-v document]
   (-> graph
     (or (get-graph out-v))
     assert-linear!
     (add-edge label out-v in-v document)
     (get-edge label out-v in-v))))

(defn add-vertex
  "Add a vertex id to the graph with or without an associated document.

  Adding a vertex with no document may have the effect of removing a document
  from an already existing vertex.

  Returns the updated graph."
  ([graph id]
   (add-vertices graph [[id nil]]))
  ([graph id document]
   (add-vertices graph [[id document]])))

(defn add-vertex!
  "The same as add-vertex, but returns the created vertex instead of returning
  the graph.

  Only works on linear graphs."
  ([graph id]
   (-> graph
     assert-linear!
     (add-vertex id)
     (get-vertex id)))
  ([graph id document]
   (-> graph
     assert-linear!
     (add-vertex id document)
     (get-vertex id))))

(defn remove-vertex
  "Delete the given vertex"
  [g v]
  (if v
    (remove-vertices g [v])
    g))


(defn remove-vertex!
  "Delete the given vertex"
  [v]
  (when v (remove-vertices (get-graph v) [v]))
  nil)

(defn add-vertices!
  "The same as add-vertices, but returns the created vertices instead of
  returning the graph.

  Only works on linear graphs."
  [g id-document-pairs]
  (let [g (add-vertices g id-document-pairs)]
    (map #(get-vertex g (first %)) id-document-pairs)))

(defn add-edges!
  "The same as add-edges, but returns the created edges instead of returning the
  graph.

  Only works on linear graphs."
  ([graph label pairs-or-triples]
   (let [g (add-edges (assert-linear! graph) label pairs-or-triples)]
     (map #(-get-edge g label (first %) (second %))
       pairs-or-triples)))
  ([graph label edge-type pairs-or-triples]
   (let [g (add-edges (assert-linear! graph) label edge-type pairs-or-triples)]
     (map #(-get-edge g label (first %) (second %))
       pairs-or-triples))))

(defn has-vertex?
  "Return true if a vertex or a vertex with the given id is present in the
  graph. Optionally restrict the search to vertices that have edges with
  specific labels."
  ([graph id]
   (if (vertex? id)
     (exists? id)
     (-has-vertex? graph id)))
  ([graph id labels]
   (if (vertex? id)
     (exists? id)
     (-has-vertex? graph id labels))))

(defn get-vertex!
  "Attempt to look up a vertex. If a default is provided, return that if the vertex is not found.

  If the vertex is not found, signal :vertex-not-found. If nothing manages the
  signal, an exception will be raised."
  ([graph id]
   (if (-has-vertex? graph id)
     (get-vertex graph id)
     ;; I'm not sure if the better default action is to return nil or raise an exception...
     (condition :vertex-not-found [graph id]
       #_(default nil)
       (error "Vertex does not exist" {:graph graph :id id}))))
  ([graph id default]
   (if (-has-vertex? graph id)
     (get-vertex graph id)
     default)))

(defn reload
  "Get the vertex from the given graph. This is useful if the vertex was
  previously loaded in a different version of a graph."
  [g v]
  (get-vertex! g (element-id v) nil))

(defn set-document
  "Replace the document for a given element or vertex id.

  Returns the updated graph"
  [graph element-or-id document]
  (set-documents graph [[element-or-id document]]))

(defn update-document
  "Update the document for a given element.

  Returns the updated graph"
  ([graph element f]
   (set-document graph element (f (get-document element))))
  ([graph element f arg & args]
   (set-document graph element (apply f (get-document element) arg args))))

(defn update-document!
  "Update the document for the given element and return the element."
  ([element f]
   (update-document (get-graph element) element f)
   element)
  ([element f arg & args]
   (apply update-document (get-graph element) element f arg args)
   element))

(defn ensure-seq
  "Returns either nil or something sequential."
  [x]
  (if (or (nil? x) (sequential? x))
    x
    [x]))

(defn vertices
  "Return all vertices or all vertices associated with edges that have the given
  labels in the graph."
  ([g] (all-vertices g))
  ([g labels]
   (distinct (mapcat #(vertices-with-edge g %) (ensure-seq labels)))))

(defn edges
  "Return all edges or all edges that have the given labels in the graph."
  ([g] (all-edges g))
  ([g labels] (all-edges g labels)))

(defn unwrap
  "Recursively unwrap any element or just return the input if it's not wrapped"
  [e]
  (if (wrappable? e)
    (-unwrap e)
    e))

(defn kinded
  "Wrap graph using this if all vertex IDs will be `(k :type id)` for features
  related to vertex types."
  [graph]
  (->KGraph graph))

(defn with-paths
  "Allow elements to track paths starting from the point this is called. Any
  element that you traverse to will retain the path that you took to get to it.

  See also:  [[with-paths]] [[paths]] [[reverse-paths]] [[with-path]] [[path]] [[path?]]
             [[no-path]] [[cyclic-path?]]
  "

  {:see-also ["paths" "reverse-paths" "with-path" "path" "path?" "no-path" "cyclic-path?"]}
  [r]
  (map with-path r))

(defn reverse-paths
  "Return an efficient path backward from the current element to every node and
  edge traversed from the point that [[with-path]] was called on the source
  element or graph.

  See also:  [[with-paths]] [[paths]] [[reverse-paths]] [[with-path]] [[path]] [[path?]]
             [[no-path]] [[cyclic-path?]]"
  [r]
  (map path r))

(defn paths
  "Return a path (actually a reversed reverse-path) for each element.

  To get much use from this you must call [[with-paths]] (or equiv)
  earlier in your traversal at the point you want the path to start.

  Prefer [[reverse-paths]] for traversals which may have extremely long paths.

  See also:  [[with-paths]] [[paths]] [[reverse-paths]] [[with-path]] [[path]] [[path?]]
             [[no-path]] [[cyclic-path?]]"
  [r]
  (map path r))

(defn label
  "Return the label for this edge"
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

(defn out-edge-count
  "Count edges pointing out of this vertex"
  ([v] (-out-edge-count v))
  ([v labels] (-out-edge-count v labels)))

(defn in-edge-count
  "Count edges pointing in to this vertex"
  ([v] (-in-edge-count v))
  ([v labels] (-in-edge-count v labels)))

(defn both-edge-count
  "Count edges on this vertex"
  ([v] (+ (-out-edge-count v) (-in-edge-count v)))
  ([v labels] (+ (-out-edge-count v labels) (-in-edge-count v labels))))

;; TraversalDirection methods

(defn followed-forward?
  "Returns true if we followed an out-edge to get to this edge."
  [e]
  (traversed-forward e))

(defn followed-reverse?
  "Returns true if we followed an in-edge to get to this edge."
  [e]
  (not (followed-forward? e)))

(defn go-back
  "Returns the vertex we used to get to this edge"
  {:see-also ["followed-forward?" "go-on" "same-v"]}
  [e]
  (if (followed-forward? e) (out-vertex e) (in-vertex e)))

(defn go-on
  "Returns the vertex we did not use to get to this edge"
  {:see-also ["followed-forward?" "go-back" "other-v"]}
  [e]
  (if (followed-forward? e) (in-vertex e) (out-vertex e)))

(defn- fast-traversal2
  "Requires that all vertices are from the same graph to work."
  ([->traversal labels r]
   (lazy-seq
     (let [r (ensure-seq r)]
       (when (seq r)
         (if (keyword? labels)
           (if-let [get-edges (->traversal (get-graph (first r)) labels)]
             (map get-edges r)
             (map (constantly []) r))
           (let [labels (ensure-seq labels)
                 get-edges* (into []
                              (keep #(->traversal (get-graph (first r)) %))
                              labels)]
             (map #(mapcat (fn [get-edges] (get-edges %)) get-edges*) r)))))))
  ([f ->traversal labels r]
   (lazy-seq
     (let [r (ensure-seq r)]
       (when (seq r)
         (if (keyword? labels)
           (if-let [get-edges (->traversal (get-graph (first r)) labels)]
             (map (comp f get-edges) r)
             (map (constantly []) r))
           (let [labels (ensure-seq labels)
                 get-edges* (into []
                              (keep #(->traversal (get-graph (first r)) %))
                              labels)]
             (map #(f (mapcat (fn [get-edges] (get-edges %)) get-edges*)) r))))))))

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
     :else (fast-traversal2 -in-edges-prepared2 labels r)))
  ([f labels r]
   (if-let [labels (ensure-seq labels)]
     (cond
       (vertex? r) [(f (-in-edges r labels))]
       (nil? r) nil
       :else (fast-traversal2 f -in-edges-prepared2 labels r))
     (cond
       (vertex? r) [(f (-in-edges r))]
       (nil? r) nil
       :else (map (comp f -in-edges) r)))))

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
     :else (fast-traversal2 -out-edges-prepared2 labels r)))
  ([f labels r]
   (if-let [labels (ensure-seq labels)]
     (cond
       (vertex? r) [(f (-out-edges r labels))]
       (nil? r) nil
       :else (fast-traversal2 f -out-edges-prepared2 labels r))
     (cond
       (vertex? r) [(f (-out-edges r))]
       (nil? r) nil
       :else (map (comp f -out-edges) r)))))

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
     (fast-traversal2
       (fn [g labels]
         (let [fi (-in-edges-prepared2 g labels)
               fo (-out-edges-prepared2 g labels)]
           (cond (and fi fo) (fn [v] (concat (fi v) (fo v)))
                 fi fi
                 fo fo)))
       labels
       r)))
  ([f labels r]
   (if-let [labels (ensure-seq labels)]
     (cond
       (vertex? r) [(f (concat (-in-edges r labels) (-out-edges r labels)))]
       (nil? r) nil
       :else
       (fast-traversal2 f
         (fn [g labels]
           (let [fi (-in-edges-prepared2 g labels)
                 fo (-out-edges-prepared2 g labels)]
             (cond (and fi fo) (fn [v] (concat (fi v) (fo v)))
                   fi fi
                   fo fo)))
         labels
         r))
     (cond
       (vertex? r) [(concat (f (-in-edges r)) (f (-out-edges r)))]
       (nil? r) nil
       :else
       (map (fn [v] (f (concat (-in-edges v) (-out-edges v)))) r)))))

(defn both-e
  "Returns a lazy seq of edges.

  If f is given, it is executed once for the edges of each vertex"
  ([r] (apply concat (both-e* (ensure-seq r))))
  ([labels r]
   (apply concat (both-e* labels (ensure-seq r))))
  ([f labels r]
   (apply concat (both-e* f labels (ensure-seq r)))))

(defn out-v
  "Returns a lazy seq of the out vertex of each edge (out)->(in)."
  [r]
  (cond
    (edge? r) [(out-vertex r)]
    (nil? r) nil
    :else (map out-vertex (ensure-seq r))))

(defn in-v
  "Returns a lazy seq of the in vertex of each edge (out)->(in)."
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

  If f is given, it is called once per collection of vertices related to each vertex in the route."
  ([r]
   (->> r in-e* (map out-v)))
  ([labels r]
   (in-e* out-v labels r))
  ([labels f r]
   (in-e* (comp f out-v) labels r)))

(defn out*
  "Returns a lazy seq of lazy seqs of vertices with edges pointing out of this vertex.

  If f is given, it is called once per collection of vertices related to each vertex in the route."
  ([r]
   (->> r out-e* (map in-v)))
  ([labels r]
   (out-e* in-v labels r))
  ([labels f r]
   (out-e* (comp f in-v) labels r)))

(defn both*
  "Returns a lazy seq of lazy seqs of vertices with edges pointing both in and out of this vertex "
  ([r] (both-e* other-v nil r))
  ([labels r] (both-e* other-v labels r))
  ([labels f r] (both-e* (comp f other-v) labels r)))

(defn both
  "Returns a lazy seq of vertices with edges pointing both in and out of this vertex "
  ([r] (apply concat (both* r)))
  ([labels r] (apply concat (both* labels r)))
  ([labels f r] (apply concat (both* labels f r))))

(defn in
  "Returns a lazy seq of vertices with edges pointing in to this vertex
  
  The arity-3 version applies `(f vertices)` to the group of vertices traversed
  to from each vertex. See [[in-sorted]] for a use of this in practice."
  ([r] (apply concat (in* r)))
  ([labels r] (apply concat (in* labels r)))
  ([labels f r] (apply concat (in* labels f r))))

(defn out
  "Returns a lazy seq of vertices with edges pointing out of this vertex.

  The arity-3 version applies `(f vertices)` to the group of vertices traversed
  to from each vertex. See [[out-sorted]] for a use of this in practice."
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

(defn documents
  "Return the document from each element"
  [r]
  (map get-document (ensure-seq r)))

(defn has-property
  "The document must be indexable, and if it is, returns true if the document
  contains the given key value pair."
  [k v r]
  (filter (fn [e] (= v (get (get-document e) k))) (ensure-seq r)))

;; other

(defn transpose
  "Transpose reverses the direction of all edges in the graph, or creates a new
  graph with only the selected edge labels, all of which are reversed.

  I haven't measured, but I think this operation is linear on nodes+edges."
  ([g]
   (forked (-transpose (linear g))))
  ([g labels]
   (forked (-transpose (linear g) labels))))


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

(defn fast-sort-by
  "Works like sort-by but creates an intermediate collection so that f is only
  called once per element.

   Much faster if f has any cost at all."
  [f coll]
  (->> coll
       (mapv (juxt f identity))
       (sort-by #(nth % 0))
       (map #(nth % 1))))

(defn group-siblings
  "Note the stated use case for this is replaced by the much more elegant
  `go-on` / `go-back` and related methods.

   For efficiently traversing through relationships like
     (source)-[has-parent]->(parent)<-[has-parent]-(dest)
   even if the parent has multiple children.

   Returns a lazy seq of lazy seqs of siblings.

   get-siblings is a function that given source returns dest.
   to-parent and from-parent are functions that when combined will traverse from source to dest.

   This is the graph-structural version of go-on and go-back allowing for
   directional traversal through arbitrary graph structures."
  {:see-also ["go-on" "go-back"]}
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

(defn siblings
  "Note the stated use case for this is replaced by the much more elegant
  `go-on` / `go-back` and related methods.

   For efficiently traversing through relationships with the same edge direction and label in relation to a parent node
     (source)-[has-parent]->(parent)<-[has-parent]-(dest)
   even if the parent has multiple members.

   get-siblings is a function that given source returns dest.
   to-parent and from-parent are functions that when combined will traverse from
   source to dest.

   This is the graph-structural version of go-on and go-back allowing for
   directional traversal through arbitrary graph structures."
  {:see-also ["go-on" "go-back"]}
  ([get-siblings r]
   (apply concat (group-siblings get-siblings r)))
  ([to-parent from-parent r]
   (apply concat (group-siblings to-parent from-parent r))))

(defn make-pairs
  "Map each element in r to a pair of [element (f element)]."
  ([f r]
   (map (fn [v] [v (f v)]) (ensure-seq r)))
  ([f0 f1 r]
   (map (fn [v] [(f0 v) (f1 v)]) (ensure-seq r))))

(defn section
  "Apply a the section route to an element and then apply f to that section of the results.

  Both f and section are functions."
  [f section r]
  (mapcat (comp f section) (ensure-seq r)))

(defn context
  "Like section, but the function f receives the element as context together with the section route."
  [f section r]
  (mapcat (fn [e] (f e (section e))) (ensure-seq r)))

(defn sorted-section
  "This is mostly just an example of how to use sections to do sorting."
  [sort-by-f section r]
  (mapcat (comp #(fast-sort-by sort-by-f %) section) (ensure-seq r)))

(defn gather
  "Collect all results into a vector containing one collection."
  ([r] (gather [] r))
  ([coll r] [(into coll r)]))

(defn spread
  "Turn a collection of collections back into a single lazy stream. See also: merge-round-robin."
  [r]
  (apply concat r))

(defn lookahead
  "Ensure that the function produces a collection of at least one item.

   Use the arity 3 version to specify that there must be at least min and/or at
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

   Use the arity 3 version to specify that there must be at least min and/or at
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

   Use the arity 3 version to specify that there must NOT be at least min
   and/or at most max items in the route. If min or max is nil that limit will
   not be enforced. The arity 3 version of neg-lookahead is not really recommended
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

(defn keyed-branch
  "Like branch, but instead of a list of functions, it takes a flat list of
  pairs of key, function. The result is a map of key to the result of the
  function for each pair against the route.

  Example:

    (keyed-branch [:passengers to-passengers :crew to-crew] ships)"
  [pairs r]
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

;;                     [emit children siblings reset-path]
(def emit-and-continue  [true   true   true   false])
(def emit               [true   false  true   false])
(def emit-and-chain     [true   true   false  false])
(def emit-and-cut       [true   false  false  false])
(def continue           [false  true   true   false])
(def chain              [false  true   false  false])
(def ignore             [false  false  true   false])
(def cut                [false  false  false  false])

(defn reset-path
  "Apply this to a control-return-value to turn on path resetting."
  [instruction]
  (update instruction 3 true))

(def control-return-values
  {:emit-and-continue  emit-and-continue
   :emit               emit
   :emit-and-chain     emit-and-chain
   :emit-and-cut       emit-and-cut
   :continue           continue
   :chain              chain
   :ignore             ignore
   :cut                cut})

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
  ([path children coll]
   (lazy-seq (extrude (*descend path children coll))))
  ([path control children coll]
   (lazy-seq (extrude (*descend path control children coll *no-result-interval* 0)))))

(defn descents
  "Descents is a variant of descend which returns the entire descent
  path as a vector rather than just the resulting element.

  Note that the descent path is not the same as using with-path to produce
  proper paths. The descent path only includes the actual elements that are
  passed into the children function in the course of operation.

  Please see `descend` for details. In descents, the initial path is not optional."
  {:see-also ["descend" "all" "deepest" "all-paths" "deepest-paths"]}
  ([path children coll]
   (lazy-seq (extrude (*descents path children coll))))
  ([path control children coll]
   (lazy-seq (extrude (*descents path control children coll *no-result-interval* 0)))))

(defn- ev-pred [f1 f2]
  (fn
    ([a]
     (and (f1 a) (f2 a)))
    ([a b]
     (and (f1 a b) (f2 a b)))))

(defn- build-all
  "Does everything just as its name implies."
  [style control cut-cycles? pred path-pred element-pred children r]
  (let [paths (when (or cut-cycles? path-pred pred (identical? descents style))
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
        style (if control
                (partial style paths control)
                (partial style paths))]
    (if pred
      (style (fn fpred1 [path e] (when (pred path e) (children e)))
             (ensure-seq r))
      (style (fn fpred2 [path e] (children e))
             (ensure-seq r)))))

(defn all
  "Produces a lazy sequence of every element in the route and all of their
  children. Cuts cycles.

  `pred` is a `(fn [path element])` that returns true to continue iterating.

  `pred` or `path-pred` may be a natural integer, meaning the maximum path
  length allowed before iterating. Note that the internal path is only the
  elements seen by the iteration and is not the same as the more complete path
  produced by `with-path`."
  ([children r]
   (build-all descend nil true nil nil nil children r))
  ([pred children r]
   (build-all descend nil true pred nil nil children r))
  ([path-pred element-pred children r]
   (build-all descend nil true nil path-pred element-pred children r)))

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

(defn- deepest-control [children] (fn [p e] (if (seq (children e)) continue emit)))

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
  ([children r]
   (build-all descents nil true nil nil nil children r))
  ([pred children r]
   (build-all descents nil true pred nil nil children r))
  ([path-pred element-pred children r]
   (build-all descents nil true nil path-pred element-pred children r)))

(defn all-paths-to
  "Produce a lazy sequence of all paths to every element where pred returns true.

  Once a path is returned, that path will be cut and no further searching will happen.

  If there are multiple paths to the same element where pred returns true, all
  of those paths will be returned.

  Cuts cycles"
  [pred children r]
  (descents (ordered-set)
    (fn control [path e] (if (pred path e) emit continue))
    (fn [path e] (when-not (path e) (children e)))
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

(defn iter
  "Repeatedly (`n` times) apply the function `children` to the route `r`."
  {:see-also ["clojure.core/iterate"]}
  [n children r]
  (reduce (fn [r _] (children r)) r (range n)))

(defn with
  "Filters the route for elements where the result of calling the function children
   (fn [e]) are equal to v. If v is a set, then check that the result of
   calling children is in the set."
  [children v r]
  (if (set? v)
    (filter (fn [e] (v (children e)))
            (ensure-seq r))
    (filter (fn [e] (= v (children e)))
            (ensure-seq r))))

(defn is
  "Filter for items in the route equal to v."
  {:see-also ["isn't"]}
  [v r]
  (filter #(= v %) (ensure-seq r)))

(defn isn't
  "Filter for items in the route not equal to v."
  {:see-also ["is"]}
  [v r]
  (filter #(not= v %) (ensure-seq r)))

(defn one-of
  "Filter for items in the route equal to one of the items in vs."
  [vs r]
  (filter (if (set? vs) vs (set vs)) r))

(defn none-of
  "Filter for items in the route equal to one of the items in vs."
  [vs r]
  (remove (if (set? vs) vs (set vs)) r))

(defn of-kind
  "Include only items matching the `kind` keyword or predicate."
  [kind-pred r]
  (if (keyword? kind-pred)
    (filter #(= kind-pred (kind %)) r)
    (filter (comp kind-pred kind) r)))

(defn id? [x]
  (instance? KindId x))

(defn with-id
  "Include only items matching the KindId or id predicate."
  [id-pred r]
  (if (or (id? id-pred) (keyword? id-pred))
    (filter #(= id-pred (element-id %)) r)
    (filter (comp id-pred element-id) r)))

(defn not-id
  "Remove items matching the KindId or id predicate."
  [id-pred r]
  (if (or (id? id-pred) (keyword? id-pred))
    (filter #(not= id-pred (element-id %)) r)
    (remove (comp id-pred element-id) r)))

(defn with-set
  "Call the function f with a set of the values in r together with either r or (f2 r).

  Example:

     (with-set filter (f->> out out) r)

  is equivalent to the following:

     (filter (into #{} r) (->> r out out))

  By using with-set, the call is chainable.

  This call is used in the Gremlin recommender sample"
  ([f r]
   (f (into #{} (ensure-seq r)) r))
  ([f f2 r]
   (f (into #{} (ensure-seq r)) (f2 r))))

(defn distinct-in
  "Use if distinct is needed within a loop or a lookahead, or if distinctness needs to
   be coordinated across multiple places in a route.

    (let [seen (atom #{})]
      (->> r (distinct-in seen) another-r (distinct-in seen)))"
  {:deprecated "pre-release"}
  ;; FIXME: I think this has some bad behavior at the boundaries that is a bit
  ;; hard to work out so should probably be rethought.
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
  "Like prevent-cycles, but raise an :on-cycle condition. The condition will by default
  raise an ex-info.

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
  preventing cycles in chains of to-one or from-one relationships.

  NOTE that it stops iteration completely on its path when any element is seen
  twice, so must be tightly bound to the cyclic path."
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
  "Returns a function wrapping the chained methods.

  Calls ensure-seq on the input, so will not work for methods expecting a single vertex."
  [& forms]
  `(fn [r#]
     (->> r# ensure-seq ~@forms)))

(defmacro ->< [& forms-then-data]
  (let [forms (butlast forms-then-data)
        data (last forms-then-data)]
    `(-> ~data ~@forms)))

(defn pluck [f coll]
  (first (filter f (ensure-seq coll))))

(defn index-by
  "Return an index of unique items.

  `->key` is a fn that returns a key for each item.
  `->val` is a fn that returns the value to be indexed by the returned key."
  ([->key coll]
   (persistent!
    (reduce (fn [idx x] (assoc! idx (->key x) x))
            (transient {})
            (ensure-seq coll))))
  ([->key ->val coll]
   (persistent!
    (reduce (fn [idx x] (assoc! idx (->key x) (->val x)))
            (transient {})
            (ensure-seq coll)))))

(defn index-by-multi
  "Return an index of unique items.

  `->keys` is a fn that returns a vector of keys for each item. The item will be keyed to each returned key individually.
  `->val` is a fn that returns the value to be indexed by the returned keys."
  ([->keys coll]
   (persistent!
    (reduce (fn [idx x] (reduce (fn [idx k] (assoc! idx k x))
                                idx
                                (->keys x)))
            (transient {})
            (ensure-seq coll))))
  ([->keys ->val coll]
   (persistent!
    (reduce (fn [idx x]
              (let [v (->val x)]
                (reduce (fn [idx k] (assoc! idx k v))
                        idx
                        (->keys x))))
            (transient {})
            (ensure-seq coll)))))

(defn group-count
  "Return a map of {item count-equal-items} or {(f item) count-equal}.

  Arity 1 is basically identical to `frequencies`."
  {:see-also ["clojure.core/frequencies" "sorted-group-count" "group-by-count"]}
  ([coll]
   (persistent!
    (reduce (fn [r item]
              (assoc! r item (inc (get r item 0))))
            (transient {})
            (ensure-seq coll))))
  ([f coll]
   (persistent!
    (reduce (fn [r item]
              (let [k (f item)]
                (assoc! r k (inc (get r k 0)))))
            (transient {})
            (ensure-seq coll)))))

(defn sorted-group-count
  "Return a map of {item count-equal-items} or {(f item) count-equal}"
  {:see-also ["group-count" "group-by-count"]}
  ([coll]
   (reduce (fn [r item]
             (assoc r item (inc (get r item 0))))
           (sorted-map)
           (ensure-seq coll)))
  ([f coll]
   (reduce (fn [r item]
             (let [k (f item)]
               (assoc r k (inc (get r k 0)))))
           (sorted-map)
           (ensure-seq coll))))

(defn group-by-count
  "Return a map of {count [all keys with that unique count]}"
  {:see-also ["group-count" "sorted-group-by-count" "group-by-count>1"]}
  ([coll]
   (persistent!
    (reduce (fn [r [k count]]
              (assoc! r count (conj (get r count #{}) k)))
            (transient {})
            (group-count coll))))
  ([f coll]
   (persistent!
    (reduce (fn [r [k count]]
              (assoc! r count (conj (get r count #{}) k)))
            (transient {})
            (group-count f coll)))))

(defn sorted-group-by-count
  "Return a map of {count [all keys with that unique count]}"
  {:see-also ["group-by-count" "group-by-count>1"]}
  ([coll]
   (reduce (fn [r [k count]]
             (assoc r count (conj (get r count #{}) k)))
           (sorted-map)
           (group-count coll)))
  ([f coll]
   (reduce (fn [r [k count]]
             (assoc r count (conj (get r count #{}) k)))
           (sorted-map)
           (group-count f coll))))

(defn group-by-count>1
  "Return a map of {count [all keys with that unique count]} where count > 1"
  {:see-also ["group-by-count"]}
  ([coll]
   (persistent!
    (reduce (fn [r [k count]]
              (if (= 1 count)
                r
                (assoc! r count (conj (get r count #{}) k))))
            (transient {})
            (group-count coll))))
  ([f coll]
   (persistent!
    (reduce (fn [r [k count]]
              (if (= 1 count)
                r
                (assoc! r count (conj (get r count #{}) k))))
            (transient {})
            (group-count f coll)))))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  {:adapted-from 'clojure.core/distinct
   :see-also "clojure.core/distinct"}
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

(defn subgraph
  "Build a graph of only the edges in the paths of the route. You must call
  with-path on elements that are fed into the part of the route that you want to
  produce a subgraph from."
  {:see-also ["with-path"]}
  [r]
  (->> r
       (mapcat path)
       (filter edge?)
       (group-by label)
       (reduce-kv (fn [g label edges]
                    (-> g
                        (add-edges label (map (fn [edge]
                                                [(element-id (in-vertex edge)) (element-id (out-vertex edge)) (get-document edge)])
                                              edges))
                        (add-vertices (keep (fn [v]
                                              (when (get-document v)
                                                [(element-id v) (get-document v)]))
                                            (both-v edges)))))
                  (build-graph))
       forked))


(defn degree
  ([v]
   (count (both-e [v])))
  ([v labels]
   (count (both-e labels [v]))))

(defn in-degree
  ([v]
   (count (in-edges v)))
  ([v labels]
   (count (in-edges v labels))))

(defn out-degree
  ([v]
   (count (out-edges v)))
  ([v labels]
   (count (out-edges v labels))))
