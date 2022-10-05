(ns fermor.path
  (:use fermor.protocols)
  (:require [clojure.pprint :refer [simple-dispatch]]))

(declare ->PEdge ->PValue ->PVertex ->PGraph ->ReverseSubpath ->VecSubpath)

(deftype PValue [value path metadata]
  Object
  (equals [a b] (= b value))
  (hashCode [e] value)

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->PValue value path m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Wrappable
  (-unwrap [e] (-unwrap value))

  Path
  (reverse-path [e]
    (lazy-seq
     (cons value (when path (reverse-path path))))))

(deftype PVertex [element path metadata]
  Object
  (equals [a b] (and (instance? PVertex b) (= element (.element ^PVertex b))))
  (hashCode [e] (.hashCode element))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->PVertex element path m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Element
  (element-id [v] (element-id element))
  (get-graph [v] (get-graph element))
  (exists? [v] (exists? element))

  HasDocument
  (has-document? [e]
    (some? (get-document e)))

  GetDocument
  (get-document [v key] (get-document (.element v) key))

  Wrappable
  (-unwrap [e] (-unwrap (.element e)))

  Vertex

  VertexEdges
  (-out-edges [v]
    (->> (-out-edges element)
         (map #(->PEdge % v nil))))
  (-out-edges [v labels]
    (->> (-out-edges element labels)
         (map #(->PEdge % v nil))))
  (-in-edges [v]
    (->> (-in-edges element)
         (map #(->PEdge % v nil))))
  (-in-edges [v labels]
    (->> (-in-edges element labels)
         (map #(->PEdge % v nil))))

  VertexEdgesPrepared
  (-out-edges-prepared [v prepared-labels]
    (->> (-out-edges-prepared element prepared-labels)
         (map #(->PEdge % v nil))))
  (-in-edges-prepared [v prepared-labels]
    (->> (-in-edges-prepared element prepared-labels)
         (map #(->PEdge % v nil))))

  Path
  (reverse-path [e]
    (lazy-seq
     (cons element (when path (reverse-path path))))))

(deftype PEdge [element path metadata]
  Object
  (equals [a b] (and (instance? PEdge b) (= element (.element ^PEdge b))))
  (hashCode [e] (.hashCode element))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->PEdge element path m nil)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Element
  (element-id [e] (element-id element))
  (get-graph [e] (get-graph element))
  (exists? [e] (exists? element))

  HasDocument
  (has-document? [e]
    (some? (get-document e)))

  GetDocument
  (get-document [e]
    (get-document (.element e)))

  Wrappable
  (-unwrap [e] (-unwrap (.element e)))

  Edge

  EdgeLabel
  (-label [e] (-label (.element e)))

  EdgeVertices
  (in-vertex [e] (PVertex. (in-vertex (.element e)) e nil))
  (out-vertex [e] (PVertex. (out-vertex (.element e)) e nil))

  Path
  (reverse-path [e]
    ^:reverse-path
    (lazy-seq
     (cons (.element e) (when (.path e) (reverse-path (.path e))))))

  TraversalDirection ;; see followed-forward?, followed-reverse?, go-back, go-on, other-v, same-v
  (traversed-forward [e] (traversed-forward (.element e))))

(deftype PGraph [graph metadata]
  Object
  (equals [a b] (and (instance? PGraph b) (= graph (.graph ^PGraph b))))
  (hashCode [e] (.hashCode graph))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->PGraph graph m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  Graph

  GraphSettings
  (-settings [g] (-settings graph))

  Wrappable
  (-unwrap [g] (-unwrap graph)))

(defn add-to-path [path x]
  (cond (vertex? x) (PVertex. x path nil)
        (edge? x) (PEdge. x path nil)
        :else (PValue. x path nil)))

(deftype ReverseSubpath [rpath metadata]
  Object
  (equals [a b] (when (path? b)
                  (= (reverse-path a) (reverse-path b))))
  (hashCode [e] (.hashCode (reverse-path e)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (->ReverseSubpath rpath m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  clojure.lang.Seqable
  (seq [this] (seq (reverse rpath)))

  clojure.lang.Sequential ;marker, no methods

  clojure.lang.Counted
  (count [_] (count rpath))

  clojure.lang.IPersistentCollection
  (cons [this val] (ReverseSubpath. (cons val rpath) nil))
  (empty [_] (->VecSubpath [] nil))
  (equiv [this o] (.equals this o))

  ISubpath
  (-subpath [e from-end]
    (ReverseSubpath. (drop from-end rpath) metadata))
  (-subpath [e from-end length]
    (ReverseSubpath. (take length (drop from-end rpath)) metadata))

  Path
  (reverse-path [e] (seq rpath)))

(deftype VecSubpath [^clojure.lang.IPersistentVector path metadata]
  Object
  (equals [a b] (when (path? b)
                  (= (reverse-path a) (reverse-path b))))
  (hashCode [e] (.hashCode (reverse-path e)))

  clojure.lang.IObj
  (withMeta [o m]
    (if (= m metadata)
      o
      (VecSubpath. path m)))

  clojure.lang.IMeta
  (meta [o] metadata)

  clojure.lang.Seqable
  (seq [this] (seq path))

  clojure.lang.Sequential ;marker, no methods

  clojure.lang.Counted
  (count [_] (count path))

  clojure.lang.IPersistentCollection
  (cons [this val] (ReverseSubpath. (cons val (reverse path)) nil))
  (empty [_] (VecSubpath. [] nil))
  (equiv [this o] (.equals this o))

  ISubpath
  (-subpath [e from-end]
    (let [new-end (max 0 (- (count path) from-end))]
      (VecSubpath. (subvec path 0 new-end) nil)))
  (-subpath [e from-end length]
    (let [new-end (max 0 (- (count path) from-end))]
      (VecSubpath. (subvec path (max 0 (- new-end length)) new-end) nil)))

  Path
  (reverse-path [e] (reverse path)))

(defn subpath
  ([e]
   (if (path? e)
     e
     (ReverseSubpath. (reverse-path e) nil)))
  ([e from-end]
   (if (<= from-end 0)
     e
     (if (subpath? e)
       (-subpath e from-end)
       (ReverseSubpath. (drop from-end (reverse-path e)) nil))))
  ([e from-end length]
   (if (subpath? e)
     (-subpath e from-end length)
     (ReverseSubpath. (take length (drop from-end (reverse-path e))) nil))))

(defn path
  "Returns a subpath object that can be compared or iterated and supports being
  trimmed from the end via the `subpath` function."
  ([]
   (->VecSubpath [] nil))
  ([e]
   (cond
     (subpath? e)
     e
     (subpath? e)
     (->ReverseSubpath (reverse-path e) nil)
     :else
     (->VecSubpath [e] nil))))

(defn same-path? [a b]
  (= (path a) (path b)))

(defn with-path
  "Begin tracking paths for a given vertex or edge.

  If e is a graph, track paths for all elements subsequently retrieved from this instance of the graph.
  Will not impact elements that have already been retrieved from the graph or elements that are retrieved
  by relation to them."
  [e]
  (cond (vertex? e) (PVertex. e nil nil)
        (edge? e) (PEdge. e nil nil)
        (graph? e) (PGraph. e nil)
        :else (PValue. e nil nil)))

(defn as-path
  "Turn a vector of arbitrary things into a path. Not lazy."
  [v]
  (->VecSubpath (vec v) nil))

(defn no-path
  "Extract the wrapped vertex or edge.

  Only unwraps one level, so will not interfere with multi-layered path tracking."
  [e]
  (cond (instance? PVertex e) (.element ^PVertex e)
        (instance? PEdge e) (.element ^PEdge e)
        (instance? PGraph e) (.graph ^PGraph e)
        (instance? PValue e) (.value ^PValue e)
        (subpath? e) (last e)
        :else e))

(defn reset-path [e]
  (condp instance? e
    PVertex (->PVertex (no-path e) nil nil)
    PEdge (->PEdge (no-path e) nil nil)
    PValue (->PValue (no-path e) nil nil)
    (with-path e)))

#_
(defn path?
  "Returns true if the given element is wrapped to track paths."
  [e]
  (subpath? e))

(defn has-path?
  "Returns true if the given element is wrapped to track paths."
  [e]
  (path? e))

(defn no-path!
  "Extracts the raw vertex or edge even if it is wrapped in multiple levels of paths."
  [e]
  (let [e' (no-path e)]
    (if (identical? e e')
      e
      (recur e'))))

(defn path-vertices [path]
  (filter vertex? path))

(defn path-edges [path]
  (filter edge? path))

(defn cyclic-path?
  "Returns any edge that is detected more than once in a path. This does not guarantee that
  the algorithm is actually in a cycle, but it is a good indicator.

  The arity-2 version will stop searching after looking at max-search edges. To be effective, however, the
  algorithm must have the opportunity to encounter an edge more than once. It must be at least 1, and a
  reasonable number may be 10 or 20."
  ([e]
   (when (path? e)
     (loop [[e & es] (reverse-path e) edges #{}]
       (when e
         (if (edge? e)
           (if (edges e)
             e
             (recur es (conj edges e)))
           (recur es edges))))))
  ([e max-search]
   (when (< max-search 1)
     (throw (ex-info "Invalid max-search argument. Must be at least 1." {:max-search max-search})))
   (when (path? e)
     (loop [[e & es] (reverse-path e) edges #{} depth max-search]
       (when e
         (if (edge? e)
           (if (edges e)
             e
             (if (= 0 depth)
               nil
               (recur es (conj edges e) (dec depth))))
           (recur es edges depth)))))))

(defn- print-path [v ^java.io.Writer w]
  (binding [*compact-edge-printing* *compact-path-printing*]
    (let [p (subpath (path v) 0 11)
          long? (= 11 (count p))
          p (if long? (next p) p)]
      (when long?
        (.write w "... "))
      (when-let [x (first p)]
        (print-method (no-path! x) w))
      (doseq [x (rest p)]
        (.write w " ")
        (print-method (no-path! x) w)))))

(defmethod print-method PVertex [v ^java.io.Writer w]
  (.write w "#path/V [")
  (print-path v w)
  (.write w "]"))

(defmethod print-method PEdge [e ^java.io.Writer w]
  (.write w "#path/E [")
  (print-path e w)
  (.write w "]"))

(defmethod print-method PValue [e ^java.io.Writer w]
  (.write w "#path/value [")
  (print-path e w)
  (.write w "]"))

(defmethod simple-dispatch PVertex [o] (print-method o *out*))
(defmethod simple-dispatch PEdge [o] (print-method o *out*))
(defmethod simple-dispatch PValue [o] (print-method o *out*))

(defmethod print-method ReverseSubpath [p ^java.io.Writer w]
  (.write w "#path/P [")
  (print-path p w)
  (.write w "]"))

(defmethod print-method VecSubpath [p ^java.io.Writer w]
  (.write w "#path/P [")
  (print-path p w)
  (.write w "]"))

(defmethod simple-dispatch ReverseSubpath [o] (print-method o *out*))
(defmethod simple-dispatch VecSubpath [o] (print-method o *out*))
