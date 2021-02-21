(ns fermor.loom-graph
  (:use fermor.protocols)
  (:require [loom.graph :as loom]
            [fermor.graph :refer [forked linear]]
            [fermor.custom-graph :as custom]))

(def ^:dynamic *edge-labels* [:loom])

(defn- to-id [node]
  (if (vertex? node)
    (element-id node)
    node))

(deftype LoomGraph [graph]
  loom/Graph
  (nodes [g] (all-vertices graph))
  (edges [g]
    (mapcat #(-out-edges % *edge-labels*)
            (all-vertices graph)))
  (has-node? [g node]
    (or (-has-vertex-document? g (to-id node))
        (-has-vertex? g (to-id node) *edge-labels*)))
  (has-edge? [g n1 n2]
    (reduce (fn [_ label]
              (boolean (-get-edge g label (to-id n1) (to-id n2))))
            false *edge-labels*))
  (successors* [g node] (map in-vertex (-out-edges node *edge-labels*)))
  (out-degree [g node] (count (-out-edges node *edge-labels*)))
  (out-edges [g node] (-out-edges node *edge-labels*))

  loom/Digraph
  (predecessors* [g node] (map out-vertex (-in-edges node *edge-labels*)))
  (in-degree [g node] (count (-in-edges node *edge-labels*)))
  (in-edges [g node] (-in-edges node *edge-labels*))
  (transpose [g] (forked (-transpose (linear g) *edge-labels*)))

  loom/WeightedGraph
  (weight* [g e]
    (if e
      (-weight e)
      ##Inf))
  (weight* [g n1 n2]
    (loom/weight* g (some #(= n2 (in-vertex %)) (-out-edges n1 *edge-labels*)))))

(deftype LoomEditableGraph [graph]
  ;; TODO
  loom/EditableGraph
  (add-nodes* [g nodes] "Add nodes to graph g. See add-nodes")
  (add-edges* [g edges] "Add edges to graph g. See add-edges")
  (remove-nodes* [g nodes] "Remove nodes from graph g. See remove-nodes")
  (remove-edges* [g edges] "Removes edges from graph g. See remove-edges")
  (remove-all [g] "Removes all nodes and edges from graph g"))

(deftype LoomEdge [edge]
  loom/Edge
  (src [e] (in-vertex edge))
  (dest [e] (out-vertex edge)))

;; Add shims to the custom graph to support the loom protocols:

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

(custom/extend-linear-graph
 loom/EditableGraph {:add-nodes* :linear-graph
                     :add-edges* :linear-graph
                     :remove-nodes* :linear-graph
                     :remove-edges* :linear-graph
                     :remove-all :linear-graph})

;; Wrap a forked graph as a loom graph

(defn as-loom-graph
  "Wrap a graph in the Loom protocols to enable fermor graphs to be used with the Loom algos."
  [g]
  (custom/wrap-graph g ->LoomGraph ->LoomEditableGraph nil ->LoomEdge))
