(ns fermor.build
  (:require [fermor.custom-graph :as custom]
            [fermor.protocols :as p]
            [fermor.graph :as graph]
            [fermor.core :as g])
  (:import fermor.graph.IEdgeGraphs
           (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph IMap Map IEdge)))

(declare ->BV ->BuildGraph ->BLG)

(defn forked-builder
  ([] (forked-builder (atom nil)))
  ([ga]
   (if (g/graph? ga)
     (forked-builder (atom nil) ga)
     (fn graph->BuildGraph [graph]
       (reset! ga graph)
       (->BuildGraph ga))))
  ([ga graph]
   (reset! ga graph)
   (->BuildGraph ga)))


(defn vertex-builder [ga]
  (fn element->BV [element]
    (->BV ga element)))

(defn refresh [graph vertex]
  (p/get-vertex graph (p/element-id vertex)))

#_
(defn builder-graph [orig-g]
  (let [ga (atom orig-g)]
    (custom/wrap-graph orig-g
      nil
      (forked-builder ga)
      nil
      nil ;;(vertex-builder ga)
      nil)))

(comment
  (let [g
        (-> (g/graph)
          g/forked
          forked-builder
          (g/add-edges :xyz [[1 2] [2 3]]))
        v (first (g/vertices g))
        _ (g/add-edges g :abc [[1 100] [2 100] [3 100]])]
    (g/out v)
    (g/linear g))

  ,)


(defmacro ^:private mutate [sym op]
  ;; This is a very nasty little macro that's just to save some repetition.
  ;; It requires the self argument to be called g and needs to be within the
  ;; implementation so that the ga atom is visible.
  `(let [~sym (p/to-linear @~'ga)]
     (prn ~sym)
     ~op
     (reset! ~'ga (p/to-forked ~sym))
     ~'g))

(defn -all-vertices [g]
  (map #(graph/->V g (g/element-id %) nil nil)
    (p/all-vertices @(.ga g))))

(deftype BuildGraph [ga]
  p/Wrappable
  (-unwrap [g] (p/-unwrap @ga))

  p/ToLinear
  (to-linear [g]
    (graph/linear @ga))

  p/ToForked
  (to-forked [g]
    (graph/forked @ga))

  IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [g]
    (._getLabels @ga))
  (_getEdgeGraph ^IGraph [g label]
    (._getEdgeGraph @ga label))
  (_removeEdgeGraph ^ILabelGraphs [g label]
    (mutate lg
      (._removeEdgeGraph lg label)))

  (_addEdgeGraph ^ILabelGraphs [g label ^IGraph edge]
    (mutate lg
      (._addEdgeGraph lg label edge)))

  p/GraphTranspose
  (-transpose [g]
    (p/-transpose g (._getLabels g)))
  (-transpose [g labels]
    ;; This will only include listed labels in the output graph
    (let [lg (p/to-linear @ga)
          lg (p/-transpose lg labels)]
      ;; can't use mutate in this case.
      (reset! ga (p/to-forked lg))
      g))

  p/AddEdges
  (add-edges [g label pairs-or-triples]
    (mutate lg
      (p/add-edges lg label pairs-or-triples)))
  (add-edges [g label edge-type pairs-or-triples]
    (mutate lg
      (p/add-edges lg label edge-type pairs-or-triples)))

  p/AddVertices
  (add-vertices [g id-document-pairs]
    (mutate lg
      (p/add-vertices g id-document-pairs)))

  p/RemoveEdges
  (remove-edges [g es]
    (mutate lg
      (p/remove-edges lg es)))

  p/RemoveVertices
  (remove-vertices [g vertices]
    (mutate lg
      (p/remove-vertices lg vertices)))

  p/RemoveDocuments
  (remove-documents [g elements]
    (mutate lg
      (p/remove-documents lg elements)))

  p/SetDocuments
  (set-documents [g element-document-pairs]
    (mutate lg
      (p/set-documents lg element-document-pairs)))

  p/HasVertex
  (-has-vertex? [g id labels]
    (p/-has-vertex? @ga id labels))

  (-has-vertex? [g id]
    (p/-has-vertex? @ga id))

  #_#_
  GetEdge
  (-get-edge [g label from-id to-id]
    (when-let [edge (._getEdgeGraph g label)]
      (try
        (do
          ;; The only way to test for edge existence seems to be to call .edge and see if it raises.
          ;; FIXME: try to find a better way to do this.
          (.edge ^IGraph edge from-id to-id)
          ;; Don't cache the document in a mutable graph.
          (->E label (->V g from-id nil nil) (->V g to-id nil nil) nil true nil))
        (catch IllegalArgumentException e
          nil))))

  p/AllVertices
  (all-vertices [g]
    (-all-vertices g))


  #_#_#_
  AllEdges
  (all-edges [g]
    (all-edges g (._getLabels ^IEdgeGraphs g)))
  (all-edges [g labels]
    (letfn [(next-edges [labels]
              (when (seq labels)
                (let [label (first labels)]
                  (if-let [^IGraph edge (._getEdgeGraph ^IEdgeGraphs g label)]
                    (lazy-seq
                      (concat
                        (map (fn [^IEdge e]
                               (->E label
                                 (->V g (.from e) nil nil)
                                 (->V g (.to e) nil nil)
                                 nil true nil))
                          (iterator-seq (.iterator (.edges edge))))
                        (next-edges (rest labels))))
                    (next-edges (rest labels))))))]
      (next-edges labels)))

  #_#_
  GetVertex
  (get-vertex [g id]
    (->V g id nil nil))

  #_#_
  Linear
  (to-forked [g]
    (->ForkedGraph (.forked (.mapValues edges
                              (reify BiFunction
                                (apply [this k v]
                                  (.forked ^IGraph v)))))
      (.forked documents)
      settings
      metadata)))
