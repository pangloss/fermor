(ns fermor.path
  (:use fermor.protocols))

; NOTE: equality is oddly strict because I cannot change equality semantics of Neo elements which are not wrapped. Change this if that changes.
;       meanwhile use core/equal?

(deftype PVertex [element path]
  Object
  (equals [a b] (and (instance? PVertex b) (= element (.element b))))
  (hashCode [e] (.hashCode element)))

(deftype PEdge [element path]
  Object
  (equals [a b] (and (instance? PEdge b) (= element (.element b))))
  (hashCode [e] (.hashCode element)))

(deftype PGraph [graph]
  Object
  (equals [a b] (and (instance? PGraph b) (= graph (.graph b))))
  (hashCode [e] (.hashCode graph)))

(defn with-path
  "Begin tracking paths for a given vertex or edge.

  If e is a graph, track paths for all elements subsequently retrieved from this instance of the graph.
  Will not impact elements that have already been retrieved from the graph or elements that are retrieved
  by relation to them."
  [e]
  (cond (vertex? e) (PVertex. e nil)
        (edge? e) (PEdge. e nil)
        (graph? e) (PGraph. e)))

(defn path?
  "Returns true if the given element is wrapped to track paths."
  [e]
  (satisfies? Path e))

(defn path
  "Return the fully realized path tracked by the vertex or edge.

   See also reverse-path."
  [e]
  (if (satisfies? Path e)
    (reverse (reverse-path e))
    [e]))

(defn no-path
  "Extract the wrapped vertex or edge.

  Only unwraps one level, so will not interfere with multi-layered path tracking."
  [e]
  (cond (instance? PVertex e) (.element ^PVertex e)
        (instance? PEdge e) (.element ^PEdge e)
        (instance? PGraph e) (.graph ^PGraph e)
        :else e))

(defn no-path!
  "Extracts the raw vertex or edge even if it is wrapped in multiple levels of paths."
  [e]
  (let [e' (no-path e)]
    (if (identical? e e')
      e
      (recur e'))))

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

(extend-type PGraph
  Graph
  Wrappable
  (-unwrap [g]
    (-unwrap (.graph g))))

(extend-type PVertex
  Element
  (element-id [v] (element-id (.element v)))
  (get-graph [v] (get-graph (.element v)))
  (get-property [v key] (get-property (.element v) key))

  Wrappable
  (-unwrap [e] (-unwrap (.element e)))

  Vertex
  (-out-edges
    ([v] (->> (-out-edges (.element v))
              (map #(PEdge. % v))))
    ([v labels]
     (->> (-out-edges (.element v) labels)
          (map #(PEdge. % v))))
    #_
    ([v _ prepared-labels]
     (->> (-out-edges (.element v) _ prepared-labels)
          (map #(PEdge. % v)))))
  (-in-edges
    ([v] (->> (-in-edges (.element v))
              (map #(PEdge. % v))))
    ([v labels]
     (->> (-in-edges (.element v) labels)
          (map #(PEdge. % v))))
    #_
    ([v _ prepared-labels]
     (->> (-in-edges (.element v) _ prepared-labels)
          (map #(PEdge. % v)))))
  Path
  (reverse-path [e]
    (lazy-seq
      (cons (.element e) (when (.path e) (reverse-path (.path e)))))))


(extend-type PEdge
  Element
  (element-id [e] (element-id (.element e)))
  (get-graph [e] (get-graph (.element e)))
  (get-property [e]
    (get-property (.element e)))

  Wrappable
  (-unwrap [e] (-unwrap (.element e)))

  Edge
  (-label [e] (-label (.element e)))
  (in-vertex [e] (PVertex. (in-vertex (.element e)) e))
  (out-vertex [e] (PVertex. (out-vertex (.element e)) e))

  Path
  (reverse-path [e]
    (lazy-seq
      (cons (.element e) (when (.path e) (reverse-path (.path e)))))))


;; a nice thing would be to show in paths when an edge is traversed in reverse, 2 possibilities: inspect the path or mark the edge when traversing it. Marking it makes other natural.

(defmethod print-method PVertex [v ^java.io.Writer w]
  (.write w "#path/V [")
  (binding [*compact-edge-printing* *compact-path-printing*]
    (let [p (path v)]
      (when-let [x (first p)]
        (print-method (no-path! x) w))
      (doseq [x (rest p)]
        (.write w " ")
        (print-method (no-path! x) w))))
  (.write w "]"))

(defmethod print-method PEdge [e ^java.io.Writer w]
  (.write w "#path/E [")
  (binding [*compact-edge-printing* *compact-path-printing*]
    (let [p (path e)]
      (when-let [x (first p)]
        (print-method (no-path! x) w))
      (doseq [x (rest p)]
        (.write w " ")
        (print-method (no-path! x) w))))
  (.write w "]"))
