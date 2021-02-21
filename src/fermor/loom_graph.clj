(ns fermor.loom-graph
  (:use fermor.protocols)
  (:require [loom.graph :as loom]
            loom.attr
            [fermor.graph :refer [vertices-with-edge forked linear]]
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

(defn- to-e [g element]
  (if (or (vertex? element) (edge? element))
    element
    (get-vertex g element)))

(defn- use-labels [x]
  (-> x -wrapper -wrapper-settings :edge-labels))

(deftype LoomGraph [graph]
  loom/Graph
  (nodes [g]
    (mapcat #(vertices-with-edge graph %) (use-labels graph)))
  (edges [g]
    (mapcat #(-out-edges % (use-labels graph))
            (all-vertices graph)))
  (has-node? [g node]
    (or (-has-vertex-document? graph (to-id node))
        (-has-vertex? graph (to-id node) (use-labels graph))))
  (has-edge? [g n1 n2]
    (reduce (fn [_ label]
              (when-let [x (-get-edge graph label (to-id n1) (to-id n2))]
                (reduced true)))
            false (use-labels graph)))
  (successors* [g node] (map in-vertex (-out-edges (to-v graph node) (use-labels graph))))
  (out-degree [g node] (count (-out-edges (to-v graph node) (use-labels graph))))
  (out-edges [g node] (-out-edges (to-v graph node) (use-labels graph)))

  loom/Digraph
  (predecessors* [g node] (map out-vertex (-in-edges (to-v graph node) (use-labels graph))))
  (in-degree [g node] (count (-in-edges (to-v graph node) (use-labels graph))))
  (in-edges [g node] (-in-edges (to-v graph node) (use-labels graph)))
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
  ;; TODO
  loom.attr/AttrGraph
  (add-attr [g node-or-edge k v]   "Add an attribute to node or edge")
  (add-attr [g n1 n2 k v]          "Add an attribute to node or edge")
  (remove-attr [g node-or-edge k]  "Remove an attribute from a node or edge")
  (remove-attr [g n1 n2 k]         "Remove an attribute from a node or edge")
  (attr [g node-or-edge k] (some-> (loom.attr/attrs g node-or-edge) (get k)))
  (attr [g n1 n2 k] (some-> (loom.attr/attrs g n1 n2) (get k)))
  (attrs [g node-or-edge] (get-document (to-e graph node-or-edge)))
  (attrs [g n1 n2] (some-> (some #(-get-edge graph % (to-id n1) (to-id n2)) (use-labels graph))
                           get-document))

  loom/Digraph
  (transpose [g] (-transpose g (use-labels graph)))

  loom/EditableGraph
  (add-nodes* [g nodes]    "Add nodes to graph g. See add-nodes")
  (add-edges* [g edges]    "Add edges to graph g. See add-edges")
  (remove-nodes* [g nodes] "Remove nodes from graph g. See remove-nodes")
  (remove-edges* [g edges] "Removes edges from graph g. See remove-edges")
  (remove-all [g]          "Removes all nodes and edges from graph g"))


(deftype LoomEdge [edge]
  loom/Edge
  (src [e] (in-vertex edge))
  (dest [e] (out-vertex edge)))

;; Add shims to the custom graph to support the loom protocols:

(do
  (custom/extend-edge
   loom/Edge {:src :vertex
              :dest :vertex})
  (custom/extend-forked-graph
   loom/Graph {:nodes [:vertex]
               :edges [:edge]
               :successors* [:vertex]
               :out-edges [:edge]})
  (custom/extend-forked-graph
   loom/Digraph {:predecessors* [:vertex]
                 :in-edges [:edge]
                 :transpose :forked-graph})
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
                          :weight/nil 1
                          :weight/no-edge ##Inf}
                         settings)]
     (custom/wrap-graph g settings ->LoomGraph ->LoomEditableGraph nil ->LoomEdge))))

