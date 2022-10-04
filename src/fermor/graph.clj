(ns fermor.graph
  (:refer-clojure :exclude [filter keep merge merge-with])
  (:require [fermor.protocols :refer :all]
            [pure-conditioning :refer [condition error default optional]]
            [clojure.pprint :refer [simple-dispatch]])
  (:import (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph IMap Map IEdge)
           (java.util.function BiFunction)
           (java.util Optional)
           (clojure.lang IMeta)))

(set! *warn-on-reflection* true)

(declare ->LinearGraph ->ForkedGraph ->V ->E
  graph-equality -get-edge-document --in-edges --out-edges -documents
  -edges -has-vertex-document? edge-graphs)

(defn linear
  "Make the graph mutable (but only therefore useable in linear code)"
  [x]
  (cond (forked? x) (to-linear x)
        (linear? x) x
        :else (condition :unknown-type-for/linear x)))

(defn forked
  "Make the graph immutable."
  [x]
  (cond (linear? x) (to-forked x)
        (forked? x) x
        :else (condition :unknown-type-for/forked x)))

(defn dag-edge
  "Provide this as the value of edge-type when calling add-edges (the first time, when the edge type
  is being created), and it will use this type of edge for all subsequent edges added with that label."
  (^IGraph [] (.linear (DirectedAcyclicGraph.)))
  (^IGraph [linear?]
   (if linear?
     (dag-edge)
     (.forked (dag-edge)))))
(defn digraph-edge
  "Provide this as the value of edge-type when calling add-edges (the first time, when the edge type
  is being created), and it will use this type of edge for all subsequent edges added with that label."
  (^IGraph [] (.linear (DirectedGraph.)))
  (^IGraph [linear?]
   (if linear?
     (digraph-edge)
     (.forked (digraph-edge)))))
(defn undirected-edge
  "Provide this as the value of edge-type when calling add-edges (the first time, when the edge type
  is being created), and it will use this type of edge for all subsequent edges added with that label."
  (^IGraph [] (.linear (io.lacuna.bifurcan.Graph.)))
  (^IGraph [linear?]
   (if linear?
     (undirected-edge)
     (.forked (undirected-edge)))))

;; This does not allow multiple edges of the same type between identical
;; vertices. That's ok for most cases but a bit inconvenient. It can be modelled
;; by using the edge document to specify edge count. If needed, multiple edges could
;; be reified in edge objects when traversing. Until I find it's really needed
;; I'm not going to do it, and even then it may be better to model it as another
;; backing edge graph like or wrapping digraph or dag.


(declare -add-vertices -add-edges -remove-vertex-documents -set-edge-documents
  vertex-ids-with-document)

(definterface IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [])
  (_getEdgeGraph ^io.lacuna.bifurcan.IGraph [label])
  (_removeEdgeGraph ^ILabelGraphs [label])
  (_addEdgeGraph ^ILabelGraphs [label ^io.lacuna.bifurcan.IGraph edges]))

(definterface IDocumentCache
  (_getDocument ^java.util.Optional [])
  (_setDocument ^IDocumentCache [^java.util.Optional p]))

(defn remove-all-edges
  "Remove all edges with the given label from the graph."
  [^IEdgeGraphs g label]
  (._removeEdgeGraph g label))

(defn- -remove-documents [g elements]
  (let [{:keys [vertices edges]}
        (group-by (fn [e]
                    (cond (vertex? e) :vertices
                          (edge? e) :edges))
                  elements)]
    (cond-> g
      (seq vertices) (-remove-vertex-documents vertices)
      (seq edges) (-set-edge-documents (map #(vector % nil) edges)))))

(defn- -set-documents [g element-document-pairs]
  (let [{:keys [vertices edges ids]}
        (group-by (fn [[e]]
                    (cond (vertex? e) :vertices
                          (edge? e) :edges
                          :else :ids))
                  element-document-pairs)]
    (cond-> g
      (seq ids) (-add-vertices ids)
      (seq vertices) (-add-vertices (map (fn [[v doc]]
                                           [(element-id v) doc])
                                         vertices))
      (seq edges) (-set-edge-documents edges))))

(defn -remove-vertices [^IEdgeGraphs g ^IMap edges documents settings metadata vertices]
  (let [ids (mapv element-id vertices)]
    (doseq [label (._getLabels g)
            id ids]
      (let [^IGraph edge (.get (.get edges label))]
        (.put edges label
          (.remove edge id)))))
  (-remove-vertex-documents g vertices)
  g)

(deftype LinearGraph [^IMap edges ^IMap documents settings metadata]
  Object
  (equals [a b] (graph-equality a (-unwrap b)))
  (hashCode [e] (hash-combine (.hashCode edges) (.hashCode documents)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->LinearGraph edges documents settings m)))

  Graph

  GraphSettings
  (-settings [g] settings)

  clojure.lang.IMeta
  (meta [o] metadata)

  IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [g]
    (vec (.keys edges)))
  (_getEdgeGraph ^IGraph [g label]
    (let [x (.get edges label)]
      (when (.isPresent x)
        (.get x))))
  (_removeEdgeGraph ^ILabelGraphs [g label]
    (let [x (.get edges label)]
      (if (.isEmpty x)
        g
        (do (.remove edges label)
            g))))
  (_addEdgeGraph ^ILabelGraphs [g label ^IGraph edge]
    (do (.put edges label edge)
        g))

  GraphTranspose
  (-transpose [g]
    (-transpose g (._getLabels g)))
  (-transpose [g labels]
    ;; This will only include listed labels in the output graph
    (LinearGraph. (reduce (fn [^IMap edges label]
                            (if-let [edge (._getEdgeGraph g label)]
                              (.put edges label (.transpose ^IGraph edge))
                              edges))
                    (.linear (Map.))
                    labels)
      documents settings nil))

  AddEdges
  (add-edges ^LinearGraph [^LinearGraph graph label pairs-or-triples]
    (-add-edges graph label pairs-or-triples))
  (add-edges ^LinearGraph [^LinearGraph graph label edge-type pairs-or-triples]
    (-add-edges graph label edge-type pairs-or-triples))

  AddVertices
  (add-vertices [g id-document-pairs]
    (-add-vertices g id-document-pairs))

  RemoveEdges
  (remove-edges [g es]
    (doseq [[label es] (group-by -label es)
            :let [^IGraph edge (._getEdgeGraph g label)]
            :when edge
            e es]
      (.unlink edge (element-id (out-vertex e)) (element-id (in-vertex e))))
    g)

  RemoveVertices
  (remove-vertices [g vertices]
    (-remove-vertices g edges documents settings metadata vertices))

  RemoveDocuments
  (remove-documents [g elements]
    (-remove-documents g elements))

  SetDocuments
  (set-documents [g element-document-pairs]
    (-set-documents g element-document-pairs))

  HasVertex
  (-has-vertex? [g id labels]
    (reduce (fn [_ label]
              (when-let [edge (._getEdgeGraph g label)]
                (when (.isPresent (.indexOf ^IGraph edge id))
                  (reduced true))))
      false labels))
  (-has-vertex? [g id]
    (or (-has-vertex-document? g id)
      (-has-vertex? g id (._getLabels g))))

  GetEdge
  (-get-edge [g label from-id to-id]
    (when-let [edge (._getEdgeGraph g label)]
      (try
        (do
          ;; The only way to test for edge existence seems to be to call .edge and see if it raises.
          ;; FIXME: try to find a better way to do this.
          (.edge ^IGraph edge from-id to-id)
          ;; Don't cache the document in a mutable graph.
          (->E label (->V g from-id nil nil) (->V g to-id nil nil) nil true nil))
        (catch IllegalArgumentException e
          nil))))

  AllVertices
  (all-vertices [g]
    (->> (edge-graphs g)
      (map #(.vertices ^IGraph (val %)))
      (apply concat (vertex-ids-with-document g))
      distinct
      (map #(->V g % nil nil))))

  AllEdges
  (all-edges [g]
    (all-edges g (._getLabels ^IEdgeGraphs g)))
  (all-edges [g labels]
    (letfn [(next-edges [labels]
              (when (seq labels)
                (let [label (first labels)]
                  (if-let [^IGraph edge (._getEdgeGraph ^IEdgeGraphs g label)]
                    (lazy-seq
                      (concat
                        (map (fn [^IEdge e]
                               (->E label
                                 (->V g (.from e) nil nil)
                                 (->V g (.to e) nil nil)
                                 nil true nil))
                          (iterator-seq (.iterator (.edges edge))))
                        (next-edges (rest labels))))
                    (next-edges (rest labels))))))]
      (next-edges labels)))

  GetVertex
  (get-vertex [g id]
    (->V g id nil nil))

  Linear
  (to-forked [g]
    (->ForkedGraph (.forked (.mapValues edges
                              (reify BiFunction
                                (apply [this k v]
                                  (.forked ^IGraph v)))))
      (.forked documents)
      settings
      metadata)))

(defn build-graph
  "Create a new linear graph.

  Possible settings:
  - :document-equality?   If `false` (default), vertices are compared for
                          equality only by id. If `true`, vertices also
                          compare based on their associated document.
  - :edge-builder         Map of {:edge-label (fn [graph ^IGraph edges from to document])}
                          The function must return the IGraph with the edge added.
                          Default: `add-unique-edge`"
  ([]
   (->LinearGraph (.linear (Map.)) (.linear (Map.)) nil nil))
  ([settings]
   (->LinearGraph (.linear (Map.)) (.linear (Map.)) settings nil))
  ([settings metadata]
   (->LinearGraph (.linear (Map.)) (.linear (Map.)) settings metadata)))

(defn IGraph->graph [^IGraph g label]
  (._addEdgeGraph ^IEdgeGraphs (build-graph) label g))

(defn- binary-op [f]
  (if (instance? java.util.function.BinaryOperator f)
    f
    (reify java.util.function.BinaryOperator
      (apply [this a b] (f a b)))))

(def ^:private standard-merge-behavior
  (reify java.util.function.BinaryOperator
    (apply [this a b] b)))

(defn add-value-semantics [graph]
  ;; Note: public DirectedAcyclicGraph(ToLongFunction<V> hashFn, BiPredicate<V, V> equalsFn) exists, too.
  (condition :todo nil
             (error "TODO: re initialize the graph so that all maps have value semantics\n"
                    "via Map(ToLongFunction<K> hashFn, BiPredicate<K,K> equalsFn)")))

(defn add-unique-edge
  "The default strategy. The edge document can be any value, and adding a
  duplicate edge will replace the old edge."
  [graph ^IGraph edges out-v in-v edge-document]
  (.link edges out-v in-v edge-document))

(def ^:private merge-weighted-edges
  (reify java.util.function.BinaryOperator
    (apply [this a b]
      (+ a b))))

(defn add-unique-weighted-edge
  "An edge builder that forces the edge-document to be a double and will update
  the edge weight with the sum of the existing weight and the new weight if the
  same edge is added again."
  [^double default-weight]
  (fn [graph ^IGraph edges out-v in-v edge-weight]
    (.link edges out-v in-v
           (double (if (number? edge-weight) edge-weight default-weight))
           merge-weighted-edges)))

(def ^:private merge-parallel-edges
  (reify java.util.function.BinaryOperator
    (apply [this a b]
      (let [count (:parallel/count a)]
        (assoc a
               :parallel/count (inc count)
               count (get b 0))))))

(defn add-parallel-edge
  "An edge builder that will attach multiple documents to an edge, simulating parallel edges."
  [graph ^IGraph edges out-v in-v edge-document]
  (.link edges out-v in-v
         {:parallel/count 1 0 edge-document}
         merge-parallel-edges))

(defn- -add-edges
  (^LinearGraph [^LinearGraph graph label pairs-or-triples]
   (-add-edges graph label nil pairs-or-triples))
  (^LinearGraph [^LinearGraph graph label edge-type pairs-or-triples]
   (-add-edges graph label edge-type pairs-or-triples
     (get-in (.settings graph) [:edge-builder label] add-unique-edge)))
  (^LinearGraph [^LinearGraph graph label edge-type pairs-or-triples edge-builder]
   (let [^IGraph edges (let [x (.get ^IMap (.edges graph) label)]
                         (if (.isEmpty x)
                           (if edge-type (edge-type) (digraph-edge))
                           (.get x)))
         edges (reduce (fn [^IGraph edges [out-v in-v edge-document]]
                         (edge-builder graph edges out-v in-v edge-document))
                 edges pairs-or-triples)]
     (.put ^IMap (.edges graph) label edges)
     graph)))

(defn- -set-edge-documents [g edge-document-pairs]
  (reduce-kv (fn [graph label pairs]
               (-add-edges g label nil
                           (map (fn [[e doc]]
                                  [(element-id (out-vertex e)) (element-id (in-vertex e)) doc])
                                pairs)
                           ;; TODO: this needs to use an alternative strategy for simulated parallel edges, etc.
                           ;; It would be good to somehow attach the update strategy to the insertion strategy.
                           ;; FIXME: for now it just replaces everything
                           add-unique-edge))
             g (group-by (comp -label first) edge-document-pairs)))

(defn- -add-vertices ^LinearGraph [^LinearGraph graph id-document-pairs]
  (let [^IMap docs (.documents graph)]
    (doseq [[id document] id-document-pairs]
      (if (some? document)
        (.put docs id document)
        (when-not (.contains docs id)
          (.put docs id nil)))))
  graph)

(defn- -remove-vertex-documents [^LinearGraph g vertices]
  (let [^IMap docs (.documents g)]
    (doseq [v vertices]
      (.remove docs (element-id v))))
  g)

(defn- -forked-set-documents [g element-document-pairs]
  (forked
   (set-documents (linear g)
                  element-document-pairs)))

(deftype ForkedGraph [^IMap edges ^IMap documents settings metadata]
  Object
  (equals [a b] (graph-equality a (-unwrap b)))
  (hashCode [e] (hash-combine (.hashCode edges) (.hashCode documents)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->ForkedGraph edges documents settings m)))

  HasVertex
  (-has-vertex? [g id labels]
    (reduce (fn [_ label]
              (when-let [edge (._getEdgeGraph g label)]
                (when (.isPresent (.indexOf ^IGraph edge id))
                  (reduced true))))
      false labels))
  (-has-vertex? [g id]
    (or (-has-vertex-document? g id)
      (-has-vertex? g id (._getLabels g))))

  GetEdge
  (-get-edge [g label from-id to-id]
    (when-let [edge (._getEdgeGraph g label)]
      (try
        (->E label (->V g from-id nil nil) (->V g to-id nil nil)
          ;; NOTE: .edge is to fetch the edge document, but if there is no edge
          ;; document it will also raise an exception. If it's possible, it
          ;; would be better if I could actually check for the edge existence.
          (Optional/ofNullable (.edge ^IGraph edge from-id to-id))
          true nil)
        (catch IllegalArgumentException e
          nil))))

  clojure.lang.IMeta
  (meta [o] metadata)

  Wrappable
  (-unwrap [g] g)

  IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [g]
    (vec (.keys edges)))
  (_getEdgeGraph ^IGraph [g label]
    (let [x (.get edges label)]
      (when (.isPresent x)
        (.get x))))
  (_removeEdgeGraph ^ILabelGraphs [g label]
    (let [x (.get edges label)]
      (if (.isEmpty x)
        g
        (->ForkedGraph (.remove edges label) documents settings nil))))
  (_addEdgeGraph ^ILabelGraphs [g label ^IGraph edge]
    (->ForkedGraph (.put edges label edge) documents settings nil))

  Graph

  GraphSettings
  (-settings [g] settings)

  AllVertices
  (all-vertices [g]
    (->> (edge-graphs g)
         (map #(.vertices ^IGraph (val %)))
         (apply concat (vertex-ids-with-document g))
         distinct
         (map #(->V g % nil nil))))

  AllEdges
  (all-edges [g]
    (all-edges g (._getLabels ^IEdgeGraphs g)))
  (all-edges [g labels]
    (letfn [(next-edges [labels]
              (when (seq labels)
                (let [label (first labels)]
                  (if-let [^IGraph edge (._getEdgeGraph ^IEdgeGraphs g label)]
                    (lazy-seq
                      (concat
                        (map (fn [^IEdge e]
                               (->E label
                                 (->V g (.from e) nil nil)
                                 (->V g (.to e) nil nil)
                                 nil true nil))
                          (iterator-seq (.iterator (.edges edge))))
                        (next-edges (rest labels))))
                    (next-edges (rest labels))))))]
      (next-edges labels)))

  GetVertex
  (get-vertex [g id]
    (->V g id nil nil))

  RemoveDocuments
  (remove-documents [g elements]
    (forked
      (remove-documents (linear g)
        (map (fn [e]
               (cond (vertex? e) e
                     (edge? e) e
                     :else (get-vertex g e)))
          elements))))

  SetDocuments
  (set-documents [g element-document-pairs]
    (-forked-set-documents g element-document-pairs))

  Forked
  (to-linear [g]
    (->LinearGraph (.mapValues (.linear edges)
                     (reify BiFunction
                       (apply [this k v]
                         (.linear ^IGraph v))))
      (.linear documents)
      settings
      metadata)))

(defn merge-with
  "Merge graphs. When edges or documents conflict, resolve conflicts using
  merge-edge or merge-doc. Those functions are called with the conflicting
  elements in the same style as [[clojure.core/merge-with]].

  May be called with either linear or forked graphs, but always returns a linear
  graph.

  Preserves the settings and metadata of the first graph."
  ([merge-edge merge-doc g1]
   (linear g1))
  ([merge-edge merge-doc g1 g2]
   (let [^LinearGraph g1 (linear (forked g1)) ;; Ensure we don't clobber g1, be set up to return a linear graph.
         ^ForkedGraph g2 (forked g2)
         edges (.merge ^IMap (.edges g1) (.edges g2)
                 (binary-op
                   (fn [^IGraph eg1 ^IGraph eg2]
                     (.merge eg1 eg2 (binary-op merge-edge)))))
         docs (.merge ^IMap (.documents g1) (.documents g2)
                (binary-op merge-doc))]
     (->LinearGraph edges docs (.settings g1) (.metadata g1))))
  ([merge-edge merge-doc g1 g2 g3 & gs]
   (reduce (partial merge-with merge-edge merge-doc) g1 (cons g2 (cons g3 gs)))))

(defn merge
  "Merge graphs. When edges or documents conflict, keep the later ones.
  Preserves the settings and metadata of the first graph."
  ([g1] g1)
  ([g1 g2]
   (merge-with standard-merge-behavior standard-merge-behavior g1 g2))
  ([g1 g2 g3 & gs]
   (reduce merge g1 (cons g2 (cons g3 gs)))))

(defn ^IMap -documents [g]
  (if (instance? ForkedGraph g)
    (.documents ^ForkedGraph g)
    (.documents ^LinearGraph g)))

(defn ^IMap -edges [g]
  (if (instance? ForkedGraph g)
    (.edges ^ForkedGraph g)
    (.edges ^LinearGraph g)))

(defn- -has-vertex-document? [g id]
  (.contains (-documents g) id))

(defn graph-settings [g]
  (if (graph-settings? g)
    (-settings g)))

(defn document-equality? [g]
  (:document-equality? (graph-settings g)))

(defn vertex-ids-with-document [g]
  (seq (.keys (-documents g))))

(defn- graph-equality [a b]
  ;; TODO: include settings in equality and hashing?
  (if-let [[edges documents] (condp instance? a
                                LinearGraph [(.edges ^LinearGraph a) (.documents ^LinearGraph a)]
                                ForkedGraph [(.edges ^ForkedGraph a) (.documents ^ForkedGraph a)]
                                false)]
    (condp instance? b
      LinearGraph (and (.equals edges (.edges ^LinearGraph b))
                       (.equals documents (.documents ^LinearGraph b)))
      ForkedGraph (and (.equals edges (.edges ^ForkedGraph b))
                       (.equals documents (.documents ^ForkedGraph b)))
      false)
    false))

(deftype E [label out-v in-v ^:unsynchronized-mutable ^java.util.Optional document used-forward metadata]
  Object
  (equals [a b]
    (let [b (-unwrap b)]
      (and (instance? E b)
           (= label (-label b))
           (= out-v (out-vertex b))
           (= in-v (in-vertex b))
           (if (document-equality? (get-graph a))
             (= document (get-document b))
             true))))

  (hashCode [e] (hash-combine (hash label) (hash-combine (hash out-v) (hash in-v))))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->E out-v in-v document used-forward m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Wrappable
  (-unwrap [e] e)

  IDocumentCache
  (_getDocument ^java.util.Optional [e] document)
  (_setDocument [e ^java.util.Optional p] (set! document p) e)

  Element
  (element-id [e] nil)
  (get-graph [e] (get-graph out-v))
  (exists? [e]
    (-get-edge (get-graph out-v) label (element-id out-v) (element-id in-v)))

  HasDocument
  (has-document? [e]
    (some? (get-document e)))

  GetDocument
  (get-document [e] (-get-edge-document e))

  Edge

  EdgeLabel
  (-label [e] label)

  EdgeVertices
  (out-vertex [e] out-v)
  (in-vertex [e] in-v)

  TraversalDirection ;; see followed-forward?, followed-reverse?, go-back, go-on, other-v, same-v
  (traversed-forward [e] used-forward))

(defn- -get-edge-document [^E e]
  ;; The document cache is only populated when elements are retrieved, and only
  ;; if it's a forked graph.
  (if-let [p ^Optional (._getDocument e)]
    (when (.isPresent p)
      (.get p))
    (let [g (get-graph (.out_v e))
          edges (.get (-edges g) (.label e))]
      (when (.isPresent edges)
        (let [edges ^IGraph (.get edges)
              edge (.edge edges
                          (element-id (.out_v e))
                          (element-id (.in_v e)))]
          edge)))))

(defn labels
  "Return a list of edge labels present in the graph"
  [^IEdgeGraphs g]
  (._getLabels g))

(defn edge-graph
  "Return the single-label graph of all edges with the given label."
  ^IGraph [^IEdgeGraphs g label]
  (._getEdgeGraph g label))

(defn edge-graphs
  "Return a map of single-label graphs, one for each of the edges in the
  graph (or just the specified edges)."
  ([^IEdgeGraphs g]
   (edge-graphs g (labels g)))
  ([^IEdgeGraphs g labels]
   (into {} (map (juxt identity #(edge-graph g %)) labels))))

;; FIXME: validate whether using extend imposes a performance penalty vs implementation within the deftype.
;; the reason to use extend is it is easier to redefine the instance methods vs direct implementation within the type.
;; Most likely this turns into a TODO: integrate all `extend` use back into the defining type.

(deftype V [graph id ^:unsynchronized-mutable ^java.util.Optional document metadata]
  Object
  (equals [a b] (let [b (-unwrap b)]
                  (and (instance? V b)
                       (= id (element-id b))
                       (if (document-equality? graph)
                         (= document (get-document b))
                         true))))
  (hashCode [e] (hash id))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->V graph id document m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Wrappable
  (-unwrap [e] e)

  IDocumentCache
  (_getDocument ^java.util.Optional [e] document)
  (_setDocument [e ^java.util.Optional p] (set! document p) e)

  Vertex ;; marker

  VertexEdges
  ;; TODO specialize 4 edge types with combinations of vertex id and vertex obj for in and out. Eliminate extra wrappers.
  (-out-edges [v]
    (--out-edges v (labels (.graph v))))
  (-out-edges [v labels]
    (--out-edges v labels))

  (-in-edges [^V v]
    (--in-edges v (labels (.graph v))))
  (-in-edges [^V v labels]
    (--in-edges v labels))

  VertexEdgesPrepared
  (-out-edges-prepared [v labels]
    (--out-edges v labels))
  (-in-edges-prepared [^V v labels]
    (--in-edges v labels))

  Element
  (element-id [e] id)
  (get-graph [e] graph)
  (exists? [e] (-has-vertex? graph id))

  HasDocument
  (has-document? [e]
    (some? (get-document e)))

  GetDocument
  (get-document [e]
    (if document
      (when (.isPresent document)
        (.get document))
      (let [p (.get (-documents graph) id)]
        (when (forked? graph)
          (set! (.document e) p))
        (when (.isPresent p)
          (.get p))))))

(defn edges-with-label?
  "Returns true if the given vertex has any edges with the given label."
  ;; FIXME : is this correct in both directions?
  ([^V v label]
   (edges-with-label? v label (edge-graph (.graph v) label)))
  ([^V v label ^IGraph edge]
   (.isPresent (.indexOf edge (.id v)))))

(defn vertices-with-edge
  "Return all vertices that have an edge with the given label."
  [graph label]
  (when-let [edge (edge-graph (-unwrap graph) label)]
    (into #{}
      (map #(->V graph % nil nil))
      (.vertices edge))))

(defn- --out-edges [^V v labels]
  (mapcat (fn [label]
            (when-let [edge (edge-graph (.graph v) label)]
              (when (edges-with-label? v label edge)
                (map #(->E label v (->V (.graph v) % nil nil) nil true nil)
                     (.out edge (.id v))))))
          labels))

(defn- --in-edges [^V v labels]
  (mapcat (fn [label]
            (when-let [edge (edge-graph (.graph v) label)]
              (when (edges-with-label? v label edge)
                (map #(->E label (->V (.graph v) % nil nil) v nil false nil)
                     (.in edge (.id v))))))
          labels))

(defn print-edge [^String as-out ^String as-in ^E e ^java.io.Writer w]
  (if *compact-edge-printing*
    (if (traversed-forward e)
      (do
        (.write w "-")
        (print-method (.label e) w)
        (.write w "->"))
      (do
        (.write w "<-")
        (print-method (.label e) w)
        (.write w "-")))
    (do
      (.write w "(")
      (when (linear? e)
        (.write w "-"))
      (if (traversed-forward e)
        (.write w as-out)
        (.write w as-in))
      (print-method (element-id (.out_v e)) w)
      (.write w " ")
      (print-method (.label e) w)
      (when-let [p (get-document e)]
        (.write w " [")
        (print-method p w)
        (.write w "]"))
      (.write w " ")
      (print-method (element-id (.in_v e)) w)
      (.write w ")"))))

(defmethod print-method E [^E e ^java.io.Writer w]
  (print-edge "e-> " "e->in " e w))

(defmethod simple-dispatch E [o]
  (print-method o *out*))

(defmethod print-method V [e ^java.io.Writer w]
  (if (linear? e)
    (.write w "(-v ")
    (.write w "(v "))
  (print-method (.id ^V e) w)
  (when-not *compact-vertex-printing*
    (when-let [p (get-document e)]
      (.write w " ")
      (print-method p w)))
  (.write w ")"))

(defmethod simple-dispatch V [o]
  (print-method o *out*))

(defn v
  "The printed representation of a vertex. If you handle the :default-graph
  condition this will point to the vertex in that graph, but if not, it will
  produce a vertex object which does not necessarily correspond to any vertex in
  any actual graph.

  If you provide a document, it will only be attached if there is no default
  graph configured or if the default graph is forked."
  ([id]
   (->V (condition :default-graph nil optional) id nil nil))
  ([id document]
   (let [g (condition :default-graph nil optional)]
     (->V g id
       (when (or (nil? g) (forked? g)) (Optional/ofNullable document))
       nil))))

(def -v v)

(defn e->
  "The printed representation of an edge from the vertex with out-id to the
  vertex in-id. If you handle the :default-graph condition this will point to
  the correct edge for that graph, but if not, it will produce an edge object
  which does not necessarily correspond to an edge in any actual graph."
  ([out-id label in-id]
   (->E label (v out-id) (v in-id) nil true nil))
  ([out-id label [document] in-id]
   (let [g (condition :default-graph nil optional)]
     (->E label (v out-id) (v in-id)
       (when (or (nil? g) (forked? g)) (Optional/ofNullable document))
       true nil))))

(defn e->in
  "The printed representation of an edge to the vertex with out-id from the
  vertex in-id. If you handle the :default-graph condition this will point to
  the correct edge for that graph, but if not, it will produce an edge object
  which does not necessarily correspond to an edge in any actual graph."
  ([out-id label in-id]
   (->E label (v out-id) (v in-id) nil false nil))
  ([out-id label [document] in-id]
   (let [g (condition :default-graph nil optional)]
     (->E label (v out-id) (v in-id)
       (when (or (nil? g) (forked? g)) (Optional/ofNullable document))
       false nil))))

(def -e-> e->)
(def -e->in e->in)
