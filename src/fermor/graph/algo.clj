(ns fermor.graph.algo
  (:require [fermor.protocols :refer [get-vertex element-id vertex?]]
            [fermor.graph :refer [edge-graph]])
  (:import (io.lacuna.bifurcan Graphs LinearList)
           (java.util.function Predicate ToDoubleFunction)
           (java.util Optional)))

(defn- ^Predicate as-predicate [f]
  (reify Predicate
    (test [this arg] (f arg))))

(defn- ^ToDoubleFunction as-tdf [f]
  (reify ToDoubleFunction
    (applyAsDouble ^double [this arg]
      ^double (f arg))))

(defn strongly-connected-components
  "Returns a set of SCCs. Each SCC is itself a set of vertex ids.."
  [graph label include-singletons?]
  (let [g (edge-graph graph label)]
    (into #{}
      (comp
        (map (fn [set] (map #(get-vertex graph %) set)))
        (map set))
      (Graphs/stronglyConnectedComponents g include-singletons?))))

(defn shortest-path
  "Return the shortest path from the start vertex to an accepted path.

  Takes 2 control functions. (accept vertex-id) and (cost edge). Adds the
  edge cost to the path cost to get to that edge. Proceeds by using the
  lowest-cost known edge.

  Returns immediately if a vertex is accepted.

  This does not seem to do the algoritm that searches from the start and end
  nodes of the graph."
  [graph label start accept cost]
  (let [g (edge-graph graph label)
        result ^Optional (if (vertex? start)
                           (Graphs/shortestPath g (element-id start)
                             (as-predicate accept)
                             (as-tdf cost))
                           (Graphs/shortestPath g
                             ^LinearList
                             (reduce (fn [^LinearList l v]
                                       (.addLast l (element-id v)))
                               (LinearList.) start)
                             (as-predicate accept)
                             (as-tdf cost)))]
    (when (.isPresent result)
      (into []
        (map #(get-vertex graph %))
        (.get result)))))

(defn strongly-connected-subgraphs [graph label include-singletons?]
  (let [g (edge-graph graph label)]
    ;; TODO: generate labels for these and recombine them as a fermor graph.
    (Graphs/stronglyConnectedSubgraphs g include-singletons?)))

(defn cycles
  "Returns a list of all circular paths in the graph."
  [graph label]
  (let [g (edge-graph graph label)]
    (into []
      (map (fn [vs] (mapv #(get-vertex graph %) vs)))
      (Graphs/cycles g))))

(defn connected-components
  "Sets of vertices, where each vertex can reach every other vertex within the set.

  Undirected graphs only."
  [graph label]
  (let [g (edge-graph graph label)]
    (into #{}
      (comp
        (map (fn [set] (map #(get-vertex graph %) set)))
        (map set))
      (Graphs/connectedComponents g))))

(defn biconnected-components
  "Sets of vertices, where each vertex can reach every other vertex within the
  set, even if a single vertex is removed.

  Undirected graphs only."
  [graph label]
  (let [g (edge-graph graph label)]
    (into #{}
      (comp
        (map (fn [set] (map #(get-vertex graph %) set)))
        (map set))
      (Graphs/biconnectedComponents g))))


(defn articulation-points [graph label]
  (let [g (edge-graph graph label)]
    (into #{}
      (map #(get-vertex graph %))
      (Graphs/articulationPoints g))))
