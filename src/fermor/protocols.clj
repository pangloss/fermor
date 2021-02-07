(ns fermor.protocols
  (:import clojure.lang.IMeta))

(def ^:dynamic *compact-vertex-printing* false)
(def ^:dynamic *compact-edge-printing* false)
(def ^:dynamic *compact-path-printing* true)

(defprotocol Graph
  (get-vertex [g id] "Find a vertex by ID. See also parse-vertex-id."))

(defprotocol MutableGraph
  (add-vertices [g id-property-pairs])
  (add-vertex [g id]
    [g id property]
    "Add a vertex. If `property` is provided, attach it to the vertex as well. If a conflicting value in a
         unique index already exists, raises an exception.")
  (add-edge [g label out-v in-v] [g label out-v in-v property]
    "Add an edge from v to in-v with the given label string.")
  (add-edges [g label pairs] [g label edge-type pairs] "Add a edges between each pair of vertices in `pairs`.")
  (set-property [g element value] "Reset an element's property to a new value, replacing the old value."))

(defprotocol Wrappable
  (-unwrap [x] "Recursively remove wrapper classes until it's an original"))

(defprotocol Element
  (element-id [e] "Return the id of the given vertex or edge.")
  (get-graph [e] "Return the graph the element is part of.")
  (get-property [e] [e k]))

#_
(defprotocol ElementProperties
  (property-keys [e] "Return a list of property keys.")
  (get-property [e key] "Get a property by its key.")
  (has-property? [e key] "Return true if a property with the given key exists."))

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

(defn ensure-seq
  "Returns either nil or something sequential."
  [x]
  (if (or (nil? x) (sequential? x))
    x
    (if (instance? IMeta x)
      (with-meta [x] (meta x))
      [x])))

(defn out-edges
  ([v] (-out-edges v))
  ([v labels] (-out-edges v labels)))

(defn in-edges
  ([v] (-in-edges v))
  ([v labels] (-in-edges v labels)))


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

(defn in*
  "Returns a lazy seq of lazy seqs of vertices with edges pointing in to this vertex.

  If f is given, it is called once for each collection of vertices related to a single vertex in the route."
  ([r]
   (->> r in-e* (map out-v)))
  ([labels r]
   (in-e* out-v labels r))
  ([labels f r]
   (in-e* #(f (out-v %)) labels r)))

(defn out*
  "Returns a lazy seq of lazy seqs of vertices with edges pointing out of this vertex.

  If f is given, it is called once for each collection of vertices related to a single vertex in the route."
  ([r]
   (->> r out-e* (map in-v)))
  ([labels r]
   (out-e* in-v labels r))
  ([labels f r]
   (out-e* #(f (in-v %)) labels r)))

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
