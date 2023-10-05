(ns fermor.graph.algo
  (:require [fermor.protocols :refer [get-vertex element-id vertex?]]
            [fermor.graph :as fg :refer [edge-graph]]
            [fermor.core :as g]
            [pure-conditioning :refer [condition error]])
  (:import (io.lacuna.bifurcan Graphs LinearList)
           (java.util.function Predicate ToDoubleFunction)
           (java.util Optional)
           (fermor.graph IEdgeGraphs)))

(set! *warn-on-reflection* true)

;; TODO: fn to convert directed labels to undirected.

(defn- ^Predicate as-predicate [f]
  (reify Predicate
    (test [this arg] (f arg))))

(defn- ^ToDoubleFunction as-tdf [f]
  (reify ToDoubleFunction
    (applyAsDouble ^double [this arg]
      ^double (f arg))))

(defn strongly-connected-components*
  "Returns a set of SCCs."
  [graph label include-singletons?]
  (let [g (edge-graph graph label)]
    (into #{}
      (comp
        (map (fn [set] (map #(get-vertex graph %) set)))
        (map set))
      (Graphs/stronglyConnectedComponents g include-singletons?))))

(defn- scc [get-successors include-singletons? stack set-of-components i L pre x]
  (loop [pre (assoc pre x i)
         L (assoc L x i)
         i (inc i)
         stack (conj stack x)
         set-of-components set-of-components
         [y & successors] (get-successors x)]
    (if y
      (cond
        (not (pre y)) ;; x -> y is a tree arc
        (let [[stack set-of-components i L pre] (scc get-successors include-singletons? stack set-of-components i L pre y)]
          (if (< (L y 0) (L x 0))
            (recur pre (assoc L x (L y 0)) i stack set-of-components successors)
            (recur pre L i stack set-of-components successors)))
        (< (pre x) (pre y 0)) ;; do nothing
        (recur pre L i stack set-of-components successors)
        ;; x -> y is a back or cross arc
        (and (< (pre y 0) (L x 0)) (some #{y} stack))
        (recur pre (assoc L x (pre y 0)) i stack set-of-components successors)
        :else
        (recur pre L i stack set-of-components successors))
      (if (= (L x 0) (pre x 0))
        (loop [C #{}
               stack stack]
          (let [z (peek stack)
                C (conj C z)
                stack (pop stack)]
            (if (= z x)
              (let [set-of-components (if (or include-singletons? (not= 1 (count C)))
                                        (conj set-of-components C)
                                        set-of-components)]
                [stack set-of-components i L pre])
              (recur C stack))))
        [stack set-of-components i L pre]))))

(defn strongly-connected-components
  "Returns a set of SCCs for the given collection of vertices and their successors.

  - vertices may be a list of vertices (unordered) or a graph. If a graph is
    given, all vertices in the graph will be used.
  - get-successors is a function that returns some immediate successor vertices.

  Returns a set of sets of vertices. Each of the sets of vertices is a strongly
  connected group."
  [vertices get-successors include-singletons?]
  (loop [stack []
         set-of-components #{}
         i 1
         L {}
         pre {}
         [x & vertices] (if (g/graph? vertices) (g/all-vertices vertices) vertices)]
    (if x
      (if (pre x)
        (recur stack set-of-components i L pre vertices)
        ;; FIXME: instead of a pile of args and returns here, use a state object with mutable primitive members
        (let [[stack set-of-components i L pre] (scc get-successors include-singletons? stack set-of-components i L pre x)]
          (recur stack set-of-components (long i) L pre vertices)))
      set-of-components)))

(defn shortest-path
  "Return the shortest path from the start vertex to an accepted path.

  Takes 2 control functions. (accept vertex-id) and (cost edge). Adds the
  edge cost to the path cost to get to that edge. Proceeds by using the
  lowest-cost known edge.

  NOTE, the control function tests the ID of the vertex, not a vertex object.

  Returns immediately if a vertex is accepted.

  This does not seem to do the algoritm that searches from the start and end
  nodes of the graph.

  Cost should return a double.

  If no path is found, returns nil. Otherwise returns a vector of vertices on the path."
  [graph label start accept cost]
  (when-let [g (edge-graph graph label)]
    (let [result ^Optional (if (vertex? start)
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
          (.get result))))))

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

  https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/

  get-successors is a function like (f->> (out [:x])) that returns successor nodes."
  [entry-node get-successors f]
  (let [seen (volatile! #{})]
    (letfn [(descend [v]
              (when-not (@seen v)
                (vswap! seen conj v)
                (concat
                  (mapcat descend (get-successors v))
                  [(f v)])))]
      (descend entry-node))))

(defn reverse-postwalk
  "This is also called RPO. The point of it is to guarantee that elements are
  visited before any of their descendents.

  See also [[postwalk]] and [[reverse-postwalk-reduce]]."
  [entry-node get-successors f]
  (let [vs (postwalk entry-node get-successors identity)]
    (map f (reverse vs))))

(defn postwalk-reduce
  "Reduce over the graph in [[postwalk]] order.

  Each element calls (f accumulator v). The return value of the callback is the
  accumulator for the next element."
  [entry-node get-successors state f]
  (let [seen (volatile! #{})]
    (letfn [(descend [state v]
              (vswap! seen conj v)
              (loop [[child & children] (get-successors v)
                     state state]
                (if (reduced? state)
                  (unreduced state)
                  (if child
                    (if (@seen child)
                      (recur children state)
                      (let [state (descend state child)]
                        (recur children state)))
                    (f state v)))))]
      (descend state entry-node))))

(defn prewalk-reduce
  "Reduce over the graph in [[prewalk]] order.

  Each element calls (f accumulator v). The return value of the callback is the
  accumulator for the next element."
  [entry-node get-successors state f]
  (let [seen (volatile! #{})]
    (letfn [(descend [state v]
              (vswap! seen conj v)
              (loop [[child & children] (get-successors v)
                     state (f state v)]
                (if (reduced? state)
                  (unreduced state) ;; early exit
                  (if child
                    (if (@seen child)
                      (recur children state)
                      (let [state (descend state child)]
                        (recur children state)))
                    state))))]
      (descend state entry-node))))

(defn reverse-postwalk-reduce
  "Reduce over the graph in [[reverse-postwalk]] order.

  Each element calls (f accumulator v). The return value of the callback is the
  accumulator for the next element."
  [entry-node get-successors state f]
  (let [vs (postwalk entry-node get-successors identity)]
    (reduce f state (reverse vs))))

(defn reverse-post-order-numbering
  "Return a map from node to its order in RPO traversal.

  Since RPO is a topological sort, this can be used to tsort nodes in a subgraph.

  See also [[reverse-postwalk]]"
  [entry-node get-successors]
  (zipmap (reverse-postwalk entry-node get-successors identity)
    (range)))

(defn post-order-numbering
  "Return a map from node to its order in PO traversal.

  See also [[postwalk]] in this namespace."
  [entry-node get-successors]
  (zipmap (postwalk entry-node get-successors identity)
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

  This structure is effectively a tree because each node points to the node
  higher in the tree (or itself for the root node).

  Based upon \"A Simple, Fast Dominance Agorithm\" by Cooper, Harvey and Kennedy"
  [entry-node get-predecessors get-successors]
  (let [intersect (idom-intersect (post-order-numbering entry-node get-successors))]
    (loop [doms {entry-node entry-node}]
      (let [doms' (reverse-postwalk-reduce entry-node get-successors doms
                    (fn [doms v]
                      (if (= entry-node v)
                        doms
                        (let [preds (get-predecessors v)
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

(defn dominator-depth
  "Returns how many steps through the map it takes to get to the entry node.

  See also [[immediate-dominators]]."
  ([entry-node get-predecessors get-successors]
   (dominator-depth entry-node
     (immediate-dominators entry-node get-predecessors get-successors)))
  ([entry-node imm-doms]
   (loop [result {}
          nodes (keys imm-doms)
          node nil
          cursor nil
          depth 0]
     (if cursor
       (if (= cursor entry-node)
         (recur (assoc result node depth) nodes nil nil 0)
         (let [dominator (imm-doms cursor)]
           (if (= cursor dominator)
             (recur (assoc result node :unknown) nodes nil nil 0)
             (recur result nodes node dominator (inc depth)))))
       (if (seq nodes)
         (recur result (rest nodes) (first nodes) (first nodes) depth)
         result)))))


(defn dominance-frontiers
  "Calculate dominance frontiers of the subgraph originating at entry-node.

  This is a bonus algo on page 9 of the paper I got the [[immediate-dominators]] algo from."
  [entry-node get-predecessors get-successors]
  (let [doms (immediate-dominators entry-node get-predecessors get-successors)
        frontiers (zipmap (keys doms) (repeat (count doms) #{}))]
    (reduce (fn [frontiers b]
              (let [preds (get-predecessors b)]
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

(defn- pre-interval [selected h
                     get-predecessors get-successors]
  (loop [A #{h}
         xform  nil
         worklist [h]]
    (if xform
      (if-let [node (first worklist)]
        (if-let [m (first (into [] xform (get-successors node)))]
          (recur (conj A m) nil (conj worklist m))
          (recur A xform (subvec worklist 1)))
        A)
      (recur A
        (comp
          (remove selected)
          (remove A)
          (filter (fn [m] (every? A (get-predecessors m))))
          (take 1))
        worklist))))

(defn intervals
  "Return a list of intervals in the graph."
  [entry-node get-predecessors get-successors]
  (loop [workset #{entry-node}
         selected #{}
         intervals []]
    (if-let [h (first workset)]
      (let [interval (pre-interval selected h get-predecessors get-successors)
            selected (into selected interval)]
        (recur
          (into (disj workset h)
            (remove selected)
            (get-successors (seq selected)))
          selected
          (conj intervals interval)))
      intervals)))

(defn loop-tree
  "Returns a map of loop head and tail to loop details.

      {[loop-head loop-tail] {:loop-num n :parent [lh lt] :depth n} ...}

  See also [[loop-info]] for a possibly more useful result set based on this algo."
  [entry-node get-predecessors get-successors]
  (let [nums (reverse-post-order-numbering entry-node get-successors)
        n (fn [v]
            (or (nums v)
              (condition :node-not-numbered {:nums nums :node v})))]
    (:loops
     (reverse-postwalk-reduce entry-node get-successors {:active-loops {}
                                                         :nesting []
                                                         :loops {}}
       (fn [acc head]
         (let [ending-loop (get-in acc [:active-loops head])
               acc (if ending-loop
                     (-> acc
                       (update :active-loops dissoc head)
                       (update :nesting #(into [] (remove #{ending-loop}) %)))
                     acc)]
           (reduce (fn [acc tail]
                     (let [nesting (vec (vals (:active-loops acc)))]
                       (-> acc
                         (update :loops assoc [head tail] {:loop-num (count (:loops acc))
                                                           :parent (last (:nesting acc))
                                                           :depth (count (:nesting acc))})
                         (update :active-loops assoc tail [head tail])
                         (update :nesting conj [head tail]))))
             acc
             (->> (get-predecessors head)
               (filter (fn after-head [tail] (< (n head) (n tail))))
               (sort-by (fn deepest-first [tail] (- (n tail))))))))))))


(defn breadth-first-nodes [get-successors r]
  ;; TODO: make this a lazy seq
  (loop [ret []
         seen #{}
         queue clojure.lang.PersistentQueue/EMPTY
         cqueue (if (g/vertex? r)
                  (conj clojure.lang.PersistentQueue/EMPTY (delay [r]))
                  (into clojure.lang.PersistentQueue/EMPTY (delay r)))]
    (cond (seq queue)
          (let [node (peek queue)]
            (recur (conj ret node) seen (pop queue) (conj cqueue (delay (get-successors node)))))
          (seq cqueue)
          (recur ret
            (into seen @(peek cqueue))
            (into queue (comp (remove seen) (distinct)) @(peek cqueue))
            (pop cqueue))
          :else
          ret)))

(defn breadth-first-reduce
  [f init get-successors r]
  (loop [state init
         seen #{}
         queue clojure.lang.PersistentQueue/EMPTY
         cqueue (if (g/vertex? r)
                  (conj clojure.lang.PersistentQueue/EMPTY (delay [r]))
                  (into clojure.lang.PersistentQueue/EMPTY (delay r)))]
    (cond (seq queue)
          (let [node (peek queue)
                state (f state node)]
            (if (reduced? state)
              (unreduced state)
              (recur state seen (pop queue) (conj cqueue (delay (get-successors node))))))
          (seq cqueue)
          (recur state
            (into seen @(peek cqueue))
            (into queue (comp (remove seen) (distinct)) @(peek cqueue))
            (pop cqueue))
          :else
          state)))

(defn non-loop-vertices-between [from to get-successors]
  (apply into #{}
    (g/descents #{}
      (fn [path element]
        (cond (path element) g/ignore
              (= element to) g/emit
              :else g/continue))
      (fn [path element] (get-successors element))
      [from])))

(defn loop-info
  "Return a map from every vertex to its loop information.

  See also [[loop-tree]]."
  [entry pred succ]
  (let [all (postwalk entry succ identity)
        tree (loop-tree entry pred succ)]
    (reduce-kv (fn [m [from to] loop-info]
                 (let [loop-info (-> loop-info
                                   (update :depth inc)
                                   (assoc :from from :to to))]
                   (reduce (fn [m v]
                             (assoc m v loop-info))
                     m
                     (non-loop-vertices-between from to succ))))
      (zipmap all (repeat (count all) {:depth 0}))
      tree)))
