(ns fermor.graph.algo
  (:require [fermor.protocols :refer [get-vertex element-id vertex?]]
            [fermor.graph :refer [edge-graph]]
            [fermor.core :as g])
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

(defn postwalk
  "Algo via Eli Bendersky

  https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/"
  [entry-node labels f]
  (let [seen (volatile! #{})]
    (letfn [(descend [v]
              (vswap! seen conj v)
              (concat
                (mapcat descend (remove @seen (g/out labels v)))
                [(f v)]))]
      (descend entry-node))))

(defn reverse-postwalk
  "This is also called RPO. The point of it is to guarantee that elements are visited before any of their descendents.

  See also [[postwalk]] and [[reverse-postwalk-reduce]]."
  [entry-node labels f]
  (let [vs (postwalk entry-node labels identity)]
    (map f (reverse vs))))

(defn postwalk-reduce [entry-node state labels f]
  (let [seen (volatile! #{})]
    (letfn [(descend [state v]
              (vswap! seen conj v)
              (loop [[child & children] (remove @seen (g/out labels v))
                     state state]
                (if child
                  (let [state (descend state child)]
                    (recur children state))
                  (f state v))))]
      (descend state entry-node))))

(defn reverse-postwalk-reduce
  "Reduce over the graph in [[reverse-postwalk]] order.

  Each element calls (f accumulator v). The return value of the callback is the
  accumulator for the next element."
  [entry-node state labels f]
  (let [vs (postwalk entry-node labels identity)]
    (reduce f state (reverse vs))))

(defn rpo-numbering
  "Return a map from node to its order in RPO traversal.

  Since RPO is a topological sort, this can be used to tsort nodes in a subgraph."
  [entry-node labels]
  (zipmap (reverse-postwalk entry-node labels identity)
    (range)))

(defn dominators
  "Return a map of dominators.

  Based upon \"A Simple, Fast Dominance Agorithm\" by Cooper, Harvey and Kennedy"
  [entry-node labels]
  (let [blocknum (rpo-numbering entry-node labels)]
    (letfn [(intersect [doms b1 b2]
              (loop [finger1 b1 finger2 b2]
                (let [n1 (blocknum finger1 -1)
                      n2 (blocknum finger2 -1)]
                  (cond (= n1 n2)
                        finger1
                        (< n1 n2)
                        (recur finger1 (doms finger2))
                        :else
                        (recur (doms finger1) finger2)))))]
      (loop [doms {entry-node entry-node}]
        (let [doms' (reverse-postwalk-reduce entry-node doms labels
                      (fn [doms v]
                        (if (= entry-node v)
                          doms
                          (let [new-idom
                                (reduce (fn [new-idom pred]
                                          (if (doms pred)
                                            (intersect doms pred new-idom)
                                            new-idom))
                                  ;; first element is the initial accumulator
                                  (g/in labels v))]
                            (assoc doms v new-idom)))))]
          (if (= doms doms')
            doms
            (recur doms')))))))
