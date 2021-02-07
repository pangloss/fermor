(ns fermor.graph
  (:refer-clojure :exclude [filter keep])
  (:require [fermor.protocols :refer :all]
            [conditions :refer [condition error default optional]]
            [clojure.pprint :refer [simple-dispatch]])
  (:import (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph Graphs ISet IMap Map Maps$Entry)
           (java.util.function BiFunction)
           (java.util Optional)
           (clojure.lang IMeta)))

(defprotocol Linear
  (to-forked [x]))

(defprotocol Forked
  (to-linear [x]))

(defn linear [x]
  (cond (satisfies? Forked x) (to-linear x)
        (satisfies? Linear x) x
        :else (condition :unknown-type-for/linear x)))

(defn forked [x]
  (cond (satisfies? Linear x) (to-forked x)
        (satisfies? Forked x) x
        :else (condition :unknown-type-for/forked x)))

(declare ->LinearGraph ->ForkedGraph ->V ->E graph-equality)

(defn dag
  (^IGraph [] (DirectedAcyclicGraph.))
  (^IGraph [linear?]
   (if linear?
     (.linear (dag))
     (dag))))
(defn digraph
  (^IGraph [] (DirectedGraph.))
  (^IGraph [linear?]
   (if linear?
     (.linear (digraph))
     (digraph))))
(defn undirected-graph
  (^IGraph [] (io.lacuna.bifurcan.Graph.))
  (^IGraph [linear?]
   (if linear?
     (.linear (undirected-graph))
     (undirected-graph))))

;; This does not allow multiple edges of the same type between identical
;; vertices. That's ok for most cases but a bit inconvenient. It can be modelled
;; by using the edge property to specify edge count. If needed, multiple edges could
;; be reified in edge objects when traversing. Until I find it's really needed
;; I'm not going to do it, and even then it may be better to model it as another
;; backing edge graph like or wrapping digraph or dag.

(definterface IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [])
  (_getEdgeGraph ^io.lacuna.bifurcan.IGraph [label])
  (_removeEdgeGraph ^ILabelGraphs [label])
  (_addEdgeGraph ^ILabelGraphs [label ^io.lacuna.bifurcan.IGraph edges]))

(definterface IPropertyCache
  (_getProperty ^java.util.Optional [])
  (_setProperty ^IPropertyCache [^java.util.Optional p]))

(deftype LinearGraph [^IMap edges ^IMap properties settings metadata]
  Object
  (equals [a b] (graph-equality a (-unwrap b)))
  (hashCode [e] (hash-combine (.hashCode edges) (.hashCode properties)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->LinearGraph edges properties settings m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [g]
    (vec (.keys edges)))
  (_getEdgeGraph ^IGraph [g label]
    (let [x (.get edges label)]
      (when-not (.isEmpty x)
        (.get x))))
  (_removeEdgeGraph ^ILabelGraphs [g label]
    (let [x (.get edges label)]
      (if (.isEmpty x)
        g
        (->LinearGraph (.remove edges label) properties settings nil))))
  (_addEdgeGraph ^ILabelGraphs [g label ^IGraph edge]
    (->LinearGraph (.put edges label edge) properties settings nil))

  Linear
  (to-forked [g]
    (->ForkedGraph (.forked (.mapValues edges
                                        (reify BiFunction
                                          (apply [this k v]
                                            (.forked ^IGraph v)))))
                   (.forked properties)
                   settings
                   metadata)))

(defn build-graph
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

(defn- -add-edges
  (^LinearGraph [^LinearGraph graph label pairs-or-triples]
   (-add-edges graph label #(dag true) pairs-or-triples))
  (^LinearGraph [^LinearGraph graph label edge-type pairs-or-triples]
   (let [^IGraph edges (let [x (.get ^IMap (.edges graph) label)]
                         (if (.isEmpty x)
                           (edge-type)
                           (.get x)))
         edges (reduce (fn [^IGraph edges [out-v in-v edge-property]]
                         (.link edges out-v in-v edge-property))
                       edges pairs-or-triples)]
     (->LinearGraph (.put ^IMap (.edges graph) label edges)
                    (.properties graph)
                    (.settings graph)
                    (.metadata graph)))))

(defn- -add-edge
  ([graph label out-v in-v]
   (-add-edges graph label [[out-v in-v]]))
  ([graph label out-v in-v property]
   (-add-edges graph label [[out-v in-v property]])))

(defn- -add-vertices ^LinearGraph [^LinearGraph graph id-property-pairs]
  (->LinearGraph (.edges graph)
                 (reduce (fn [^IMap props [id property]]
                           (if property
                             (.put props id property)
                             (if (.contains props id)
                               props
                               (.put props id nil))))
                         (.properties graph)
                         id-property-pairs)
                 (.settings graph)
                 (.metadata graph)))

(defn- -add-vertex
  (^LinearGraph [^LinearGraph graph id]
   (-add-vertices graph [[id nil]]))
  (^LinearGraph [^LinearGraph graph id property]
   (-add-vertices graph [[id property]])))

(extend LinearGraph
  MutableGraph
  {:add-edge #'-add-edge
   :add-edges #'-add-edges
   :add-vertices #'-add-vertices
   :add-vertex #'-add-vertex
   :set-property #'-add-vertex})

(deftype ForkedGraph [^IMap edges ^IMap properties settings metadata]
  Object
  (equals [a b] (graph-equality a (-unwrap b)))
  (hashCode [e] (hash-combine (.hashCode edges) (.hashCode properties)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->ForkedGraph edges properties settings m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Wrappable
  (-unwrap [g] g)

  IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [g]
    (vec (.keys edges)))
  (_getEdgeGraph ^IGraph [g label]
    (let [x (.get edges label)]
      (when-not (.isEmpty x)
        (.get x))))
  (_removeEdgeGraph ^ILabelGraphs [g label]
    (let [x (.get edges label)]
      (if (.isEmpty x)
        g
        (->ForkedGraph (.remove edges label) properties settings nil))))
  (_addEdgeGraph ^ILabelGraphs [g label ^IGraph edge]
    (->ForkedGraph (.put edges label edge) properties settings nil))

  Graph
  (get-vertex [g id]
    (->V g id nil nil))

  Forked
  (to-linear [g]
    (->LinearGraph (.mapValues (.linear edges)
                               (reify BiFunction
                                 (apply [this k v]
                                   (.linear ^IGraph v))))
                   (.linear properties)
                   settings
                   metadata)))

(defn graph-settings [g]
  (condp instance? g
    ForkedGraph (.settings ^ForkedGraph g)
    LinearGraph (.settings ^LinearGraph g)))

(defn property-equality? [g]
  (:property-equality? (graph-settings g)))

(defn- graph-equality [a b]
  ;; TODO: include settings in equality and hashing?
  (if-let [[edges properties] (condp instance? a
                                LinearGraph [(.edges ^LinearGraph a) (.properties ^LinearGraph a)]
                                ForkedGraph [(.edges ^ForkedGraph a) (.properties ^ForkedGraph a)]
                                false)]
    (condp instance? b
      LinearGraph (and (.equals edges (.edges ^LinearGraph b))
                       (.equals properties (.properties ^LinearGraph b)))
      ForkedGraph (and (.equals edges (.edges ^ForkedGraph b))
                       (.equals properties (.properties ^ForkedGraph b)))
      false)
    false))

(deftype E [label out-v in-v ^:unsynchronized-mutable ^java.util.Optional property used-forward metadata]
  Object
  (equals [a b] (let [b (-unwrap b)]
                  (and (instance? E b)
                       (= label (-label b))
                       (= out-v (out-vertex b))
                       (= in-v (in-vertex b))
                       (if (property-equality? (get-graph a))
                         (= property (get-property b))
                         true))))

  (hashCode [e] (hash-combine label (hash-combine out-v in-v)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->E out-v in-v property used-forward m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Wrappable
  (-unwrap [e] e)

  IPropertyCache
  (_getProperty ^java.util.Optional [e] property)
  (_setProperty [e ^java.util.Optional p] (set! property p) e)

  Edge
  (-label [e] label)
  (out-vertex [e] out-v)
  (in-vertex [e] in-v)

  TraversalDirection ;; see ->?, <-?, go-back, go-on, other-v, same-v
  (traversed-forward [e] used-forward))

(defn- -get-edge-property [^E e]
  (if-let [p ^Optional (._getProperty e)]
    (when-not (.isEmpty p)
      (.get p))
    (let [g ^ForkedGraph (get-graph (.out_v e))
          edges (.get ^IMap (.edges g) (.label e))]
      (when-not (.isEmpty edges)
        (let [edges ^IGraph (.get edges)
              edge (.edge edges
                          (element-id (.out_v e))
                          (element-id (.in_v e)))]
          edge)))))

;; FIXME: validate whether using extend imposes a performance penalty vs implementation within the deftype.
;; the reason to use extend is it is easier to redefine the instance methods vs direct implementation within the type.
;; Most likely this turns into a TODO: integrate all `extend` use back into the defining type.
(extend E
  Element
  {:element-id (constantly nil)
   :get-graph (fn get-graph [^E e] (get-graph (.out_v e)))
   :get-property #'-get-edge-property})

(defmethod print-method E [^E e ^java.io.Writer w]
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
        (.write w "(e-> ")
        (.write w "(e<- "))
      (print-method (element-id (.out_v e)) w)
      (.write w " ")
      (print-method (.label e) w)
      (when-let [p (get-property e)]
        (.write w " [")
        (print-method p w)
        (.write w "]"))
      (.write w " ")
      (print-method (element-id (.in_v e)) w)
      (.write w ")"))))

(defmethod simple-dispatch E [o]
  (print-method o *out*))

(deftype V [^ForkedGraph graph id ^:unsynchronized-mutable ^java.util.Optional property metadata]
  Object
  (equals [a b] (let [b (-unwrap b)]
                  (and (instance? V b)
                       (= id (element-id b))
                       (if (property-equality? graph)
                         (= property (get-property b))
                         true))))
  (hashCode [e] (hash id))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->V graph id property m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Wrappable
  (-unwrap [e] e)

  IPropertyCache
  (_getProperty ^java.util.Optional [e] property)
  (_setProperty [e ^java.util.Optional p] (set! property p) e)

  Element
  (element-id [e] id)
  (get-graph [e] graph)
  (get-property [e]
    (if property
      (when-not (.isEmpty property)
        (.get property))
      (let [p (.get ^IMap (.properties graph) id)]
        (set! (.property e) p)
        (when-not (.isEmpty p)
          (.get p))))))

(defn labels [^IEdgeGraphs g]
  (._getLabels g))

(defn edge-graph ^IGraph [^IEdgeGraphs g label]
  (._getEdgeGraph g label))

(defn edge-graphs
  ([^IEdgeGraphs g]
   (edge-graphs g (labels g)))
  ([^IEdgeGraphs g labels]
   (map (juxt identity #(edge-graph g %)) labels)))

(defn edges-with-label?
  ;; FIXME : is this correct in both directions?
  ([^V v label]
   (edges-with-label? v label (edge-graph (.graph v) label)))
  ([^V v label ^IGraph edge]
   (.isPresent (.indexOf edge (.id v)))))

;; TODO specialize 4 edge types with combinations of vertex id and vertex obj for in and out. Eliminate extra wrappers.

(defn- --out-edges
  ([^V v]
   (--out-edges v (labels (.graph v))))
  ([^V v labels]
   (--out-edges v nil labels))
  ([^V v _ labels]
   ;; this specialization will be make more sense when/if I bring back abstract label support. More useful for neo4j, etc.
   (mapcat (fn [label]
             (when-let [edge (edge-graph (.graph v) label)]
               (when (edges-with-label? v label edge)
                 (map #(->E label v (->V (.graph v) % nil nil) nil true nil)
                      (.out edge (.id v))))))
           labels)))

(defn- --in-edges
  ([^V v]
   (--in-edges v (labels (.graph v))))
  ([^V v labels]
   (--in-edges v nil labels))
  ([^V v _ labels]
   (mapcat (fn [label]
             (when-let [edge (edge-graph (.graph v) label)]
               (when (edges-with-label? v label edge)
                 (map #(->E label (->V (.graph v) % nil nil) v nil false nil)
                      (.in edge (.id v))))))
           labels)))

(defn vertices-with-edge [^ForkedGraph graph label]
  (when-let [edge (edge-graph graph label)]
    (map #(->V graph % nil nil) (.vertices edge))))

(extend V
  Vertex
  {:-in-edges --in-edges
   :-out-edges --out-edges})

(defmethod print-method V [e ^java.io.Writer w]
  (if *compact-vertex-printing*
    (do
      (.write w "(v ")
      (print-method (.id ^V e) w)
      (.write w ")"))
    (do
      (.write w "(v ")
      (print-method (.id ^V e) w)
      (when-let [p (get-property e)]
        (.write w " ")
        (print-method p w))
      (.write w ")"))))

(defmethod simple-dispatch V [o]
  (print-method o *out*))

(defn v
  ([id]
   (->V (condition :default-graph nil optional) id nil nil))
  ([id property]
   (->V (condition :default-graph nil optional) id (Optional/ofNullable property) nil)))

(defn e->
  ([out-id label in-id]
   (->E label (v out-id) (v in-id) nil true nil))
  ([out-id label [property] in-id]
   (->E label (v out-id) (v in-id) (Optional/ofNullable property) true nil)))

(defn e<-
  ([in-id label out-id]
   (->E label (v out-id) (v in-id) nil false nil))
  ([in-id label [property] out-id]
   (->E label (v out-id) (v in-id) (Optional/ofNullable property) false nil)))

(comment
  (get-graph (e-> 1 :xx [{:a 1 :b 2}] 4))
  (get-graph (v 1))
  (require '[fermor.path :as p])

  (def g    (-> (build-graph)
                (add-edges :testing dag (partition 3 1 (range 100000)))
                (add-edges :blogging dag (partition 2 1 (range 100000 0 -1)))
                (add-vertices (partition 2 1 (range 100000)))
                (add-edge :boo 4 999 {:a 1})
                time))

  (= (hash g) (hash (forked g)))
  (= (hash g) (hash (add-edge g :xxxxxxx 4 998 {:a 2})))

  (time
   (-> (build-graph)
       (add-edges :testing dag (partition 3 1 (range 100)))
       (add-edges :blogging dag (partition 2 1 (range 100 0 -1)))
       (add-vertices (partition 2 1 (range 1000)))
       (add-edge :boo 4 999 {:a 1})
       forked
       (get-vertex 4)
       (p/with-path)
       (->>
        in
        in in
        out out out out)))

 ,)

(comment

  ,)
