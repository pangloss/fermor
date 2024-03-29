(ns fermor.protocols
  (:require [clojure.pprint :refer [simple-dispatch]])
  (:import [clojure.lang Murmur3]))

(set! *warn-on-reflection* true)

;; You probably want to use set-config to change these globally.
(def ^:dynamic *compact-vertex-printing* true)
(def ^:dynamic *compact-edge-printing* false)
(def ^:dynamic *compact-path-printing* true)

(def config-keys
  "Available keys for `set-config` and the vars they set"
  (atom
   {:compact-vertex-printing #'*compact-vertex-printing*
    :compact-edge-printing #'*compact-edge-printing*
    :compact-path-printing* #'*compact-path-printing*}))

(defn set-config!
  "A convenient way to alter default global configuration settings residing in
  dynamic vars. The available keys are in `config-keys`.

  Example:
    (set-config! {:compact-edge-printing true})"
  [settings]
  (doseq [[k v] settings]
    (when-let [setting (@config-keys k)]
      (cond (var? setting)
            (alter-var-root setting (constantly v))
            (instance? clojure.lang.Atom setting)
            (reset! setting v)))))

#_ ;; Replaced this with a lighter weight variant on defrecord. If it's problematic, change it back!
(defrecord KindId [kind id])

(deftype KindId [kind id ^int ^:unsynchronized-mutable _hash ^int ^:unsynchronized-mutable _hasheq]
  ;; These hash and equals methods are expensive in defrecord due to the cost of
  ;; pretending the record is a map. This implementation attempts to be
  ;; identical other than omitting the fallback map
  clojure.lang.IHashEq
  (hasheq [this] (if (zero? _hasheq)
                   (let [h (int (bit-xor 280812713 #_(hash 'fermor.protocols.KindId)
                                  (Murmur3/hashUnordered {:kind kind :id id})))]
                     (set! _hasheq h)
                     h)
                   _hasheq))
  (hashCode [this] (if (zero? _hash)
                     (let [h (int (+
                                    (bit-or (hash :kind) (hash kind))
                                    (bit-or (hash :id) (hash id))))]
                       (set! _hash h)
                       h)
                     _hash))
  (equals [this other]
    (if (identical? this other)
      true
      (if (instance? KindId other)
        (if (= id (.id ^KindId other))
          (= kind (.kind ^KindId other))
          false)
        false)))

  clojure.lang.ILookup
  clojure.lang.IKeywordLookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k else] 
    (case k :kind kind :id id))
  (getLookupThunk [this k]
    (let [gclass (class this)]
      (case k
        :kind (reify clojure.lang.ILookupThunk
                (get [thunk gtarget]
                  (if (identical? (class gtarget) gclass)
                    (. ^KindId gtarget -kind)
                    thunk)))
        :id (reify clojure.lang.ILookupThunk
              (get [thunk gtarget]
                (if (identical? (class gtarget) gclass)
                  (. ^KindId gtarget -id)
                  thunk)))
        nil))))

(defn ->KindId [kind id]
  (new KindId kind id 0 0))

(defn- write-parens [^java.io.Writer w f]
  ;; This is because parinfer-rust is a buggy mess.
  (.write w "(") (f) (.write w ")"))

(defmethod print-method KindId
  [^KindId k ^java.io.Writer w]
  (write-parens w
    (fn []
      (.write w "id ")
      (print-method (.kind k) w)
      (.write w " ")
      (print-method (.id k) w))))

(defmethod simple-dispatch KindId [o]
  (print-method o *out*))

(defprotocol Kind
  (kind [v]))

(defprotocol WeightedEdge
  (-weight [e]))

(defprotocol Linear)

(defprotocol Forked)

(defprotocol ToForked
  (to-forked [x]))

(defprotocol ToLinear
  (to-linear [x]))

(defprotocol ISubpath
  (-subpath [x from-end] [x from-end length]))

(defprotocol Graph)

(defprotocol GraphSettings
  (-settings [g]))

(defprotocol AllVertices
  (all-vertices [g] [g kind]))

(defprotocol AllEdges
  (all-edges [g] [g labels]))

(defprotocol GetVertex
  (get-vertex [g id] [g kind id]
    "Find a vertex by ID. See also parse-vertex-id. By default does not check
    that the vertex exists. See `-has-vertex?` and `get-vertex!`"))

(defprotocol HasVertex
  (-has-vertex? [g id] [g id labels]
    "Return true if the vertex is present in the graph. Optionally restrict the
    search to vertices that have edges with specific labels.
    "))

(defprotocol GetEdge
  (-get-edge [g label from-id to-id]
    "If the graph contains an edge between a pair of vertex ids with the given
    label, return it.
    "))

(defprotocol GraphTranspose
  (-transpose [g] [g labels]))

(defprotocol AddVertices
  (add-vertices [g id-document-pairs]
    "The second best way to add multiple vertices to a graph after `add-edges`,
    but this way allows you to associate a document to the vertex.
    "))

(defprotocol RemoveVertices
  (remove-vertices [g vertices]))

(defprotocol AddEdges
  (add-edges
    [g label pairs-or-triples]
    [g label edge-type pairs-or-triples]
    "Add a edges between each pair of vertices in pairs.

    pairs-or-triples: Either a pair of vertex ids [from-id to-id] or include an
    edge document [from-id to-id edge-document]

    edge-type: optionally specify one of the following edge type functions:
    - dag-edge
    - digraph-edge (the default)
    - undirected-edge
     "))

(defprotocol RemoveEdges
  (remove-edges [g edges]))

(defprotocol HasDocument
  (has-document? [element]
    "Return true if the element has a document attached.
     "))

(defprotocol GetDocument
  (get-document [e] [e k] "Get the document object for the element.
     "))

(defprotocol SetDocuments
  (set-documents [g element-value-pairs]
    "Reset an element's document to a new value, replacing the old value.

     Note that you can use a map for the document to achieve the typical document set
     attached to an element. Using atoms for the document or anything else you like
     is also possible.

     Mutation must happen against the graph rather than the element being
     changed due to the structure of immutable graphs.
     "))

(defprotocol RemoveDocuments
  (remove-documents [g elements]
    "Remove the document associated with each of the given elements

     Mutation must happen against the graph rather than the element being
     changed due to the structure of immutable graphs.
     "))

(defprotocol Wrappable
  (-unwrap [x]
     "Recursively remove wrapper classes until it's an original
     "))

(defprotocol Element
  (element-id [e] "Return the id of the given vertex or edge.")
  (get-graph [e] "Return the graph the element is part of.")
  (exists? [e] "Return true if the element exists"))

(defprotocol Vertex)

(defprotocol VertexEdges
  (-out-edges [v]
    [v labels]
    "Return a lazy seq of edges out of the vertex. Labels is a collection of strings.
    ")
  (-in-edges [v]
    [v labels]
    "Return a lazy seq of edges in to the vertex. Labels is a collection of strings.
    "))

(defprotocol VertexEdgeCount
  (-out-edge-count [v] [v labels]
    "Return number of edges out of the vertex. Labels is a collection of strings.
    ")
  (-in-edge-count [v] [v labels]
    "Return number of edges in to the vertex. Labels is a collection of strings.
    "))

(defprotocol VertexEdgesPrepared
  "This specialization will be make more sense when/if I bring back abstract
  label support. More useful for neo4j, etc."
  (-out-edges-prepared [v labels])
  (-in-edges-prepared [v labels]))

(defprotocol GraphEdgesPrepared
  (-out-edges-prepared2 [g label])
  (-in-edges-prepared2 [g label]))

(defprotocol Edge)

(defprotocol EdgeLabel
  (-label [e] "Return the edge label."))

(defprotocol EdgeVertices
  (in-vertex [e] "Return the in vertex of the edge (out)-->(in).")
  (out-vertex [e] "Return the out vertex of the edge (out)-->(in)."))

(defprotocol TraversalDirection
  (traversed-forward [e]))

(defprotocol Path
  (reverse-path [e] "Get a lazy sequence of the path in reverse.

   Because some paths may be extremely long, the protocol only specifies
   reverse-path, which can be lazily generated from the vertex at the tail of the path."))

(def ^:private PLinear (:on-interface Linear))
(def ^:private PForked (:on-interface Forked))
(def ^:private PGraph (:on-interface Graph))
(def ^:private PVertex (:on-interface Vertex))
(def ^:private PEdge (:on-interface Edge))
(def ^:private PElement (:on-interface Element))
(def ^:private PPath (:on-interface Path))
(def ^:private PSubpath (:on-interface ISubpath))
(def ^:private PWrappable (:on-interface Wrappable))
(def ^:private PGraphSettings (:on-interface GraphSettings))

(defn graph?
  "Returns true if x is a graph."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PGraph (class x))))

(defn linear?
  "Returns true if x is a linear graph."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PLinear (class x))))

(defn forked?
  "Returns true if x is a forked graph."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PForked (class x))))

(defn vertex?
  "Returns true if x is a vertex."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PVertex (class x))))

(defn edge?
  "Returns true if x is an edge."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PEdge (class x))))

(defn element?
  "Returns true if x is either a vertex or an edge."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PElement (class x))))

(defn path?
  "Returns true if x is a path."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PPath (class x))))

(defn subpath?
  "Returns true if x is a subpath."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PSubpath (class x))))

(defn wrappable?
  "Returns true if x is wrappable."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PWrappable (class x))))

(defn graph-settings?
  "Returns true if x is wrappable."
  [x]
  (and (some? x) (.isAssignableFrom ^Class PGraphSettings (class x))))

(extend-type Object
  Wrappable
  (-unwrap [x] x))

(extend-type nil
  Wrappable
  (-unwrap [x] x))

(defn graph
  "Return the graph associated with the given element. If x is a graph, return x."
  [x]
  (cond (graph? x) x
        (element? x) (get-graph x)))

;; Kind Id:

(defn id
  "Create an ID object that also incorporates a type.

  Enables a sort of vertex typing."
  [kind id]
  (when id
    (->KindId kind id)))

(defn k
  "Synonym for `id` Create an ID object that also incorporates a type."
  [kind id]
  (->KindId kind id))

(defn lookup
  "Find a vertex for the given KindId in the graph."
  [kid g]
  (get-vertex g kid))
