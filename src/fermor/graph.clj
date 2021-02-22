(ns fermor.graph
  (:refer-clojure :exclude [filter keep])
  (:require [fermor.protocols :refer :all]
            [conditions :refer [condition error default optional]]
            [clojure.pprint :refer [simple-dispatch]])
  (:import (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph Graphs ISet IMap Map Maps$Entry)
           (java.util.function BiFunction)
           (java.util Optional)
           (clojure.lang IMeta)))

(defn linear [x]
  (cond (satisfies? Forked x) (to-linear x)
        (satisfies? Linear x) x
        :else (condition :unknown-type-for/linear x)))

(defn forked [x]
  (cond (satisfies? Linear x) (to-forked x)
        (satisfies? Forked x) x
        :else (condition :unknown-type-for/forked x)))

(declare ->LinearGraph ->ForkedGraph ->V ->E graph-equality -get-edge-document --in-edges --out-edges)

(defn dag-edge
  (^IGraph [] (.linear (DirectedAcyclicGraph.)))
  (^IGraph [linear?]
   (if linear?
     (dag-edge)
     (.forked (dag-edge)))))
(defn digraph-edge
  (^IGraph [] (.linear (DirectedGraph.)))
  (^IGraph [linear?]
   (if linear?
     (digraph-edge)
     (.forked (digraph-edge)))))
(defn undirected-edge
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


(declare -add-vertices -add-edges -remove-vertex-documents -set-edge-documents)

(definterface IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [])
  (_getEdgeGraph ^io.lacuna.bifurcan.IGraph [label])
  (_removeEdgeGraph ^ILabelGraphs [label])
  (_addEdgeGraph ^ILabelGraphs [label ^io.lacuna.bifurcan.IGraph edges]))

(definterface IDocumentCache
  (_getDocument ^java.util.Optional [])
  (_setDocument ^IDocumentCache [^java.util.Optional p]))

(deftype LinearGraph [^IMap edges ^IMap documents settings metadata]
  Object
  (equals [a b] (graph-equality a (-unwrap b)))
  (hashCode [e] (hash-combine (.hashCode edges) (.hashCode documents)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->LinearGraph edges documents settings m)))

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
        (->LinearGraph (.remove edges label) documents settings nil))))
  (_addEdgeGraph ^ILabelGraphs [g label ^IGraph edge]
    (->LinearGraph (.put edges label edge) documents settings nil))

  GraphTranspose
  (-transpose [g]
    (-transpose g (._getLabels g)))
  (-transpose [g labels]
    (LinearGraph. (reduce (fn [^IMap edges label]
                            (if-let [edge (._getEdgeGraph g label)]
                              (.put edges label (.transpose edge))
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
    (->LinearGraph (->> (group-by -label es)
                        (reduce-kv (fn [^IMap edges label es]
                                     (if-let [^IGraph edge (._getEdgeGraph g label)]
                                       (.put edges label
                                             (reduce (fn [^IGraph edge e]
                                                       (.unlink edge (element-id (out-vertex e)) (element-id (in-vertex e))))
                                                     edge
                                                     es))
                                       edges))
                                   edges))
                   documents settings metadata))

  RemoveVertices
  (remove-vertices [g vertices]
    (-remove-vertex-documents
     (->LinearGraph (reduce (fn [^IMap edges label]
                              (reduce (fn [^IGraph edge v]
                                        (.remove edge (element-id v)))
                                      (._getEdgeGraph g label)
                                      vertices))
                            edges
                            (._getLabels g))
                    documents settings metadata)
     vertices))

  RemoveDocuments
  (remove-documents [g elements]
    (let [{vertices true edges false} (group-by vertex?)]
      (cond-> g
        (seq vertices) (-remove-vertex-documents g vertices)
        (seq edges) (-set-edge-documents g (map #(vector % nil) edges)))))

  SetDocuments
  (set-documents [g element-document-pairs]
    (let [{vertices true edges false} (group-by (comp vertex? first))]
      (cond-> g
        (seq vertices) (-add-vertices g vertices)
        (seq edges) (-set-edge-documents g edges))))

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
     (->LinearGraph (.put ^IMap (.edges graph) label edges)
                    (.documents graph)
                    (.settings graph)
                    (.metadata graph)))))


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
  (->LinearGraph (.edges graph)
                 (reduce (fn [^IMap props [id document]]
                           (if (some? document)
                             (.put props id document)
                             (if (.contains props id)
                               props
                               (.put props id nil))))
                         (.documents graph)
                         id-document-pairs)
                 (.settings graph)
                 (.metadata graph)))

(defn- -remove-vertex-documents [^LinearGraph g vertices]
  (->LinearGraph (.edges g)
                 (reduce (fn [^IMap documents v]
                           (.remove documents (element-id v)))
                         (.documents g)
                         vertices)
                 (.settings g)
                 (.metadata g)))

(defn- -has-vertex-document? [^LinearGraph g id]
  (.isPresent (.get ^IMap (.documents g) id)))

(defn vertex-ids-with-document [^ForkedGraph g]
  (seq (.keys (.documents g))))

(declare edge-graphs)

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
                (when (.isPresent (.indexOf edge id))
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
             (Optional/ofNullable (.edge edge from-id to-id))
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
         (map #(.vertices (val %)))
         (apply concat (vertex-ids-with-document g))
         distinct
         (map #(->V g % nil nil))))

  GetVertex
  (get-vertex [g id]
    (->V g id nil nil))

  Forked
  (to-linear [g]
    #dbg
    (->LinearGraph (.mapValues (.linear edges)
                               (reify BiFunction
                                 (apply [this k v]
                                   (.linear ^IGraph v))))
                   (.linear documents)
                   settings
                   metadata)))

(defn graph-settings [g]
  (if (satisfies? GraphSettings g)
    (-settings g)))

(defn document-equality? [g]
  (:document-equality? (graph-settings g)))

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

  TraversalDirection ;; see ->?, <-?, go-back, go-on, other-v, same-v
  (traversed-forward [e] used-forward))

(defn- -get-edge-document [^E e]
  (if-let [p ^Optional (._getDocument e)]
    (when (.isPresent p)
      (.get p))
    (let [g ^ForkedGraph (get-graph (.out_v e))
          edges (.get ^IMap (.edges g) (.label e))]
      (when (.isPresent edges)
        (let [edges ^IGraph (.get edges)
              edge (.edge edges
                          (element-id (.out_v e))
                          (element-id (.in_v e)))]
          edge)))))

(defn labels [^IEdgeGraphs g]
  (._getLabels g))

(defn edge-graph ^IGraph [^IEdgeGraphs g label]
  (._getEdgeGraph g label))

(defn edge-graphs
  ([^IEdgeGraphs g]
   (edge-graphs g (labels g)))
  ([^IEdgeGraphs g labels]
   (into {} (map (juxt identity #(edge-graph g %)) labels))))

;; FIXME: validate whether using extend imposes a performance penalty vs implementation within the deftype.
;; the reason to use extend is it is easier to redefine the instance methods vs direct implementation within the type.
;; Most likely this turns into a TODO: integrate all `extend` use back into the defining type.

(deftype V [^ForkedGraph graph id ^:unsynchronized-mutable ^java.util.Optional document metadata]
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

  HasDocument
  (has-document? [e]
    (some? (get-document e)))

  GetDocument
  (get-document [e]
    (if document
      (when (.isPresent document)
        (.get document))
      (let [p (.get ^IMap (.documents graph) id)]
        (set! (.document e) p)
        (when (.isPresent p)
          (.get p))))))

(defn edges-with-label?
  ;; FIXME : is this correct in both directions?
  ([^V v label]
   (edges-with-label? v label (edge-graph (.graph v) label)))
  ([^V v label ^IGraph edge]
   (.isPresent (.indexOf edge (.id v)))))

(defn vertices-with-edge [graph label]
  (when-let [edge (edge-graph (-unwrap graph) label)]
    (map #(->V graph % nil nil) (.vertices edge))))

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

(defn print-edge [as-out as-in ^E e ^java.io.Writer w]
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
  (print-edge "(e-> " "(e<- " e w))

(defmethod simple-dispatch E [o]
  (print-method o *out*))

(defmethod print-method V [e ^java.io.Writer w]
  (.write w "(v ")
  (print-method (.id ^V e) w)
  (when-not *compact-vertex-printing*
    (when-let [p (get-document e)]
      (.write w " ")
      (print-method p w)))
  (.write w ")"))

(defmethod simple-dispatch V [o]
  (print-method o *out*))

(defn v
  ([id]
   (->V (condition :default-graph nil optional) id nil nil))
  ([id document]
   (->V (condition :default-graph nil optional) id (Optional/ofNullable document) nil)))

(defn e->
  ([out-id label in-id]
   (->E label (v out-id) (v in-id) nil true nil))
  ([out-id label [document] in-id]
   (->E label (v out-id) (v in-id) (Optional/ofNullable document) true nil)))

(defn e<-
  ([out-id label in-id]
   (->E label (v out-id) (v in-id) nil false nil))
  ([out-id label [document] in-id]
   (->E label (v out-id) (v in-id) (Optional/ofNullable document) false nil)))
