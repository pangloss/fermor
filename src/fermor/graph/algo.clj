(ns fermor.graph.algo
  (:require [fermor.protocols :refer [get-vertex element-id vertex?]]
            [fermor.graph :as fg :refer [edge-graph]]
            [fermor.core :as g])
  (:import (io.lacuna.bifurcan Graphs LinearList)
           (java.util.function Predicate ToDoubleFunction)
           (java.util Optional)
           (fermor.graph IEdgeGraphs)))

;; TODO: fn to convert directed labels to undirected.

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

(defn strongly-connected-subgraphs
  "Calculates a set of subgraphs which are then added to the graph with "
  [graph label include-singletons? result-labels]
  (let [g (edge-graph graph label)]
    (g/forked
      (reduce (fn [g [label edge-graph]]
                (fg/merge g (fg/IGraph->graph edge-graph label)))
        (g/linear graph)
        (map vector
          result-labels
          (Graphs/stronglyConnectedSubgraphs g include-singletons?))))))

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
              (when-not (@seen v)
                (vswap! seen conj v)
                (concat
                  (mapcat descend (g/out labels v))
                  [(f v)])))]
      (descend entry-node))))

(defn reverse-postwalk
  "This is also called RPO. The point of it is to guarantee that elements are
  visited before any of their descendents.

  See also [[postwalk]] and [[reverse-postwalk-reduce]]."
  [entry-node labels f]
  (let [vs (postwalk entry-node labels identity)]
    (map f (reverse vs))))

(defn postwalk-reduce
  "Reduce over the graph in [[postwalk]] order.

  Each element calls (f accumulator v). The return value of the callback is the
  accumulator for the next element."
  [entry-node labels state f]
  (let [seen (volatile! #{})]
    (letfn [(descend [state v]
              (vswap! seen conj v)
              (loop [[child & children] (g/out labels v)
                     state state]
                (if child
                  (if (@seen child)
                    (recur children state)
                    (let [state (descend state child)]
                      (recur children state)))
                  (f state v))))]
      (descend state entry-node))))

(defn prewalk-reduce
  "Reduce over the graph in [[prewalk]] order.

  Each element calls (f accumulator v). The return value of the callback is the
  accumulator for the next element."
  [entry-node labels state f]
  (let [seen (volatile! #{})]
    (letfn [(descend [state v]
              (vswap! seen conj v)
              (loop [[child & children] (g/out labels v)
                     state (f state v)]
                (if child
                  (if (@seen child)
                    (recur children state)
                    (let [state (descend state child)]
                      (recur children state)))
                  state)))]
      (descend state entry-node))))

(defn reverse-postwalk-reduce
  "Reduce over the graph in [[reverse-postwalk]] order.

  Each element calls (f accumulator v). The return value of the callback is the
  accumulator for the next element."
  [entry-node labels state f]
  (let [vs (postwalk entry-node labels identity)]
    (reduce f state (reverse vs))))

(defn reverse-post-order-numbering
  "Return a map from node to its order in RPO traversal.

  Since RPO is a topological sort, this can be used to tsort nodes in a subgraph.

  See also [[reverse-postwalk]]"
  [entry-node labels]
  (zipmap (reverse-postwalk entry-node labels identity)
    (range)))

(defn post-order-numbering
  "Return a map from node to its order in PO traversal.

  See also [[postwalk]] in this namespace."
  [entry-node labels]
  (zipmap (postwalk entry-node labels identity)
    (range)))

(defn- idom-intersect [blocknum]
  (fn intersect [doms b1 b2]
    (loop [finger1 b1 finger2 b2]
      (when (and finger1 finger2)
        (let [n1 (blocknum finger1)
              n2 (blocknum finger2)]
          (cond (= n1 n2)
                finger1
                (< n1 n2)
                (recur (doms finger1) finger2)
                :else
                (recur finger1 (doms finger2))))))))

(defn immediate-dominators
  "Return a map of immediate dominators.

  Based upon \"A Simple, Fast Dominance Agorithm\" by Cooper, Harvey and Kennedy"
  [entry-node labels]
  (let [intersect (idom-intersect (post-order-numbering entry-node labels))]
    (loop [doms {entry-node entry-node}]
      (prn doms)
      (let [doms' (reverse-postwalk-reduce entry-node labels doms
                    (fn [doms v]
                      (if (= entry-node v)
                        doms
                        (let [preds (g/in labels v)
                              new-idom (first (filter doms preds))
                              new-idom
                              (reduce (fn [new-idom pred]
                                        (if (doms pred)
                                          (intersect doms pred new-idom)
                                          new-idom))
                                new-idom
                                (remove #{new-idom} preds))]
                          (assoc doms v new-idom)))))]
        (if (= doms doms')
          doms
          (recur doms'))))))


(defn dominance-frontiers
  "Calculate dominance frontiers of the subgraph originating at entry-node.

  This is a bonus algo on page 9 of the paper I got the [[immediate-dominators]] algo from."
  [entry-node labels]
  (let [doms (immediate-dominators entry-node labels)
        frontiers (zipmap (keys doms) (repeat #{}))]
    (reduce (fn [frontiers b]
              (let [preds (g/in labels b)]
                (if (next preds) ;; |preds| >= 2
                  (reduce (fn [frontiers p]
                            (loop [runner p
                                   frontiers frontiers]
                              (if (= runner (doms b))
                                frontiers
                                (recur (doms runner) (update frontiers runner conj b)))))
                    frontiers
                    preds)
                  frontiers)))
      frontiers
      (keys doms))))

(defn- pre-interval [selected h labels]
  (loop [A #{h}
         xform  nil
         worklist [h]]
    (if xform
      (if-let [node (first worklist)]
        (if-let [m (first (into [] xform (g/out labels node)))]
          (recur (conj A m) nil (conj worklist m))
          (recur A xform (subvec worklist 1)))
        A)
      (recur A
        (comp
          (remove selected)
          (remove A)
          (filter (fn [m] (every? A (g/in labels m))))
          (take 1))
        worklist))))

(defn intervals
  "Return a list of intervals in the graph."
  [entry-node labels]
  (loop [workset #{entry-node}
         selected #{}
         intervals []]
    (if-let [h (first workset)]
      (let [interval (pre-interval selected h labels)
            selected (into selected interval)]
        (recur
          (into (disj workset h)
            (remove selected)
            (g/out labels (seq selected)))
          selected
          (conj intervals interval)))
      intervals)))


(defn loop-tree [entry-node labels]
  (let [n (reverse-post-order-numbering entry-node labels)]
    (:loops
     (prewalk-reduce entry-node labels {:active-loops {}
                                        :nesting []
                                        :loops {}}
       (fn [acc head]
         (let [ending-loop (get-in acc [:active-loops head])
               acc (if ending-loop
                     (-> acc
                       (update :active-loops dissoc head)
                       (update :nesting #(into [] (remove #{ending-loop}) %)))
                     acc)]
           (->> (g/in labels head)
             (filter (fn [tail] (< (n head) (n tail))))
             (sort-by (fn [tail] (- (n tail))))
             (reduce (fn [acc tail]
                       (let [nesting (vec (vals (:active-loops acc)))]
                         (-> acc
                           (update :loops assoc [head tail] {:loop-num (count (:loops acc))
                                                             :parent (last (:nesting acc))
                                                             :depth (count (:nesting acc))})
                           (update :active-loops assoc tail [head tail])
                           (update :nesting conj [head tail]))))
               acc))))))))
