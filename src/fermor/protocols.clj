(ns fermor.protocols
  (:require [clojure.pprint :refer [simple-dispatch]]))

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

(defrecord KindId [kind id])

(defmethod print-method KindId [^KindId k ^java.io.Writer w]
  (.write w "(id ")
  (print-method (.kind k) w)
  (.write w " ")
  (print-method (.id k) w)
  (.write w ")"))

(defmethod simple-dispatch KindId [o]
  (print-method o *out*))

(defprotocol Kind
  (kind [v]))

(defprotocol Linear
  (to-forked [x]))

(defprotocol Forked
  (to-linear [x]))

(defprotocol Graph
  (all-vertices [g] [g kind])
  (get-vertex [g id] [g kind id] "Find a vertex by ID. See also parse-vertex-id."))

(defprotocol MutableGraph
  (add-vertices [g id-document-pairs]
    "The second best way to add multiple vertices to a graph after `add-edges`,
    but this way allows you to associate a document to the vertex.")
  (add-vertex
    [g id]
    [g id document]
    "Add a vertex. If `document` is provided, attach it to the vertex as well.")
  (add-edge
    [g label out-v in-v]
    [g label out-v in-v document]
    "Add an edge from v to in-v with the given label string.")
  (add-edges
    [g label pairs]
    [g label edge-type pairs]
    "Add a edges between each pair of vertices in `pairs`.")
  (set-document [g element value]
    "Reset an element's document to a new value, replacing the old value.

     Note that you can use a map for the document to achieve the typical document set
     attached to an element. Using atoms for the document or anything else you like
     is also possible."))

(defprotocol Wrappable
  (-unwrap [x] "Recursively remove wrapper classes until it's an original"))

(defprotocol Element
  (element-id [e] "Return the id of the given vertex or edge.")
  (get-graph [e] "Return the graph the element is part of.")
  (get-document [e] [e k] "Get the document object for the element."))

(defprotocol Vertex
  (-out-edges [v]
    [v labels]
    [v _ labels]
    "Return a lazy seq of edges out of the vertex. Labels is a collection of strings.")
  (-in-edges [v]
    [v labels]
    [v _ labels]
    "Return a lazy seq of edges in to the vertex. Labels is a collection of strings."))

(defprotocol Edge
  (-label [e] "Return the edge label.")
  (in-vertex [e] "Return the in vertex of the edge (in)-->(out).")
  (out-vertex [e] "Return the out vertex of the edge (in)-->(out)."))

(defprotocol TraversalDirection
  (traversed-forward [e]))

(defprotocol Path
  (reverse-path [e] "Get a lazy sequence of the path in reverse.

   For searching within very long paths, this method may be faster than `path."))

(defn graph?
  "Returns true if x is a graph."
  [x]
  (satisfies? Graph x))

(defn vertex?
  "Returns true if x is a vertex."
  [x]
  (and x (satisfies? Vertex x)))

(defn edge?
  "Returns true if x is an edge."
  [x]
  (and x (satisfies? Edge x)))

(defn element?
  "Returns true if x is either a vertex or an edge."
  [x]
  (and x (satisfies? Element x)))

(extend-type Object
  Wrappable
  (-unwrap [x] x))

(extend-type nil
  Wrappable
  (-unwrap [x] x))

(defn graph
  "Return the graph associated with the given element. If x is a graph, return x."
  [x]
  (cond (satisfies? Graph x) x
        (satisfies? Element x) (get-graph x)))

;; Kind Id:

(defn id [kind id]
  (->KindId kind id))

(defn k "synonym for `id`" [kind id]
  (->KindId kind id))

(defn lookup [kid g]
  (get-vertex g kid))
