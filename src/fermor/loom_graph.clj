(ns fermor.loom-graph
  (:use fermor.protocols)
  (:require [loom.graph :as loom]
            loom.attr
            [fermor.graph :refer [vertices-with-edge vertex-ids-with-document forked linear]]
            [fermor.custom-graph :as custom :refer [-wrapper -wrapper-settings]]
            [fermor.core :as g]))

(defn- to-id [node]
  (if (vertex? node)
    (element-id node)
    node))

(defn- to-v [g node]
  (if (vertex? node)
    node
    (get-vertex g node)))

(defn- use-labels [x]
  (-> x -wrapper -wrapper-settings :edge-labels))

(defn- to-e [g element]
  (if (or (vertex? element) (edge? element))
    element
    (if (vector? element)
      (some #(apply -get-edge g % element)
            (use-labels g))
      (get-vertex g element))))

(defn- as-pair [edge]
  [(element-id (out-vertex edge)) (element-id (in-vertex edge))])

(deftype LoomGraph [graph]
  loom/Graph
  (nodes [g]
    (concat (map element-id (mapcat #(vertices-with-edge (-unwrap graph) %) (use-labels graph)))
            (vertex-ids-with-document (-unwrap graph))))
  (edges [g]
    (mapcat #(map as-pair (-out-edges % (use-labels graph)))
            (all-vertices graph)))
  (has-node? [g node]
    (boolean
     (or (has-document? (to-v graph node))
         (-has-vertex? graph (to-id node) (use-labels graph)))))
  (has-edge? [g n1 n2]
    (reduce (fn [_ label]
              (if-let [x (-get-edge graph label (to-id n1) (to-id n2))]
                (reduced true)
                false))
            false (use-labels graph)))
  (successors* [g node] (map (comp element-id in-vertex) (-out-edges (to-v graph node) (use-labels graph))))
  (out-degree [g node] (count (-out-edges (to-v graph node) (use-labels graph))))
  (out-edges [g node] (map as-pair (-out-edges (to-v graph node) (use-labels graph))))

  loom/Digraph
  (predecessors* [g node] (map (comp element-id out-vertex) (-in-edges (to-v graph node) (use-labels graph))))
  (in-degree [g node] (count (-in-edges (to-v graph node) (use-labels graph))))
  (in-edges [g node] (map as-pair (-in-edges (to-v graph node) (use-labels graph))))
  (transpose [g] (forked (-transpose (linear graph) (use-labels graph))))

  loom/WeightedGraph
  (weight* [g e]
    (if e
      (or (get-document e) (:weight/nil (-wrapper-settings (-wrapper graph))))
      (:weight/no-edge (-wrapper-settings (-wrapper graph)))))
  (weight* [g n1 n2]
    (loom/weight* g (some #(-get-edge graph % (to-id n1) (to-id n2))
                          (use-labels graph))))

  loom.attr/AttrGraph
  (attr [g node-or-edge k] (some-> (loom.attr/attrs g node-or-edge) (get k)))
  (attr [g n1 n2 k] (some-> (loom.attr/attrs g n1 n2) (get k)))
  (attrs [g node-or-edge] (get-document (to-e graph node-or-edge)))
  (attrs [g n1 n2] (some-> (some #(-get-edge graph % (to-id n1) (to-id n2)) (use-labels graph))
                           get-document)))

(deftype LoomEditableGraph [graph]
  loom.attr/AttrGraph
  (add-attr [g node-or-edge k v]
    (when-let [e (to-e graph node-or-edge)]
      (set-documents graph [[e (assoc (get-document e) k v)]])))
  (add-attr [g n1 n2 k v]
    (when-let [e (to-e graph [n1 n2])]
      (set-documents graph [[e (assoc (get-document e) k v)]])))
  (remove-attr [g node-or-edge k]
    (when-let [e (to-e graph node-or-edge)]
      (set-documents graph [[e (dissoc (get-document e) k)]])))
  (remove-attr [g n1 n2 k]
    (when-let [e (to-e graph [n1 n2])]
      (set-documents graph [[e (dissoc (get-document e) k)]])))
  (attr [g node-or-edge k] (some-> (loom.attr/attrs g node-or-edge) (get k)))
  (attr [g n1 n2 k] (some-> (loom.attr/attrs g n1 n2) (get k)))
  (attrs [g node-or-edge] (get-document (to-e graph node-or-edge)))
  (attrs [g n1 n2] (some-> (some #(-get-edge graph % (to-id n1) (to-id n2)) (use-labels graph))
                           get-document))

  loom/Digraph
  (transpose [g] (-transpose g (use-labels graph)))

  loom/EditableGraph
  (add-nodes* [g nodes]
    (-> (linear graph)
        (add-vertices (map #(vector % {}) nodes))
        #_forked))
  (add-edges* [g edges]
    (-> (linear graph)
        (add-edges (first (use-labels graph)) edges)
        #_forked))
  (remove-nodes* [g nodes]
    (-> (linear graph)
        (remove-vertices (filter #(loom/has-node? g %) nodes))
        #_forked))
  (remove-edges* [g edges]
    (-> (linear graph)
        (remove-edges (filter #(loom/has-edge? g (loom/src %) (loom/dest %)) edges))
        #_forked))
  (remove-all [g]
    (let [g (-> (linear graph)
                (remove-edges (loom/edges g))
                #_forked)]
      (-> (linear graph)
          (remove-documents (loom/nodes g))
          #_forked))))


;; Add shims to the custom graph to support the loom protocols:

(do
  (custom/extend-edge
   loom/Edge {})
  (custom/extend-forked-graph
   loom/Graph {})
  (custom/extend-forked-graph
   loom/Digraph {:transpose :forked-graph})
  (custom/extend-forked-graph
   loom/WeightedGraph {})
  (custom/extend-forked-graph
   loom.attr/AttrGraph {})

  (custom/extend-linear-graph
   loom.attr/AttrGraph {})
  (custom/extend-linear-graph
   loom/EditableGraph {:add-nodes* :linear-graph
                       :add-edges* :linear-graph
                       :remove-nodes* :linear-graph
                       :remove-edges* :linear-graph
                       :remove-all :linear-graph}))

;; Wrap a forked graph as a loom graph

(defn as-loom-graph
  "Wrap a graph in the Loom protocols to enable fermor graphs to be used with the Loom algos."
  ([g]
   (as-loom-graph g {}))
  ([g settings]
   (let [settings (merge {:edge-labels [:loom]
                          :weight/nil 1.0
                          :weight/no-edge ##Inf}
                         settings)]
     (custom/wrap-graph g settings ->LoomGraph ->LoomEditableGraph nil nil))))
