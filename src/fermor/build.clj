(ns fermor.build
  (:require [fermor.protocols :as p]
            [fermor.graph :as graph])
  (:import fermor.graph.IEdgeGraphs
           (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph IEdge)))

(defprotocol ResetGraph
  (-reset-graph [builder g]
    "Change the underlying graph for a builder. Use with caution as this will be a bit confusing."))

(declare ->BuilderGraph)

(defn builder
  "Create a new Builder Graph from an existing graph."
  ([graph]
   (builder (atom nil) graph))
  ([ga graph]
   (reset! ga (p/to-forked graph))
   (->BuilderGraph ga)))

(defmacro ^:private mutate [sym op]
  ;; This is a very nasty little macro that's just to save some repetition.
  ;; It requires the self argument to be called g and needs to be within the
  ;; implementation so that the ga atom is visible.
  `(let [~sym (p/to-linear @~'ga)]
     (prn ~sym)
     ~op
     (reset! ~'ga (p/to-forked ~sym))
     ~'g))

(deftype BuilderGraph [ga]
  Object
  (equals [a b] (.equals @ga b))
  (hashCode [e] (.hashCode @ga))

  clojure.lang.IObj
  (withMeta [o m]
    (swap! ga #(.withMeta % m))
    o)

  p/Graph

  p/GraphSettings
  (-settings [g] (p/-settings @ga))

  clojure.lang.IMeta
  (meta [o] (meta @ga))

  p/Wrappable
  (-unwrap [g] (p/-unwrap @ga))

  p/ToLinear
  (to-linear [g]
    (graph/linear @ga))

  p/ToForked
  (to-forked [g]
    (graph/forked @ga))

  ResetGraph
  (-reset-graph [builder g]
    (reset! ga g))

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
    ;; NOTE: unlike other functions, this one returns a new independent graph.
    (let [lg (p/to-linear @ga)
          lg (p/-transpose lg labels)]
      ;; can't use mutate in this case.
      (builder (p/to-forked lg))))

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

  p/GetEdge
  (-get-edge [g label from-id to-id]
    (when-let [edge (._getEdgeGraph g label)]
      (try
        (do
          ;; The only way to test for edge existence seems to be to call .edge and see if it raises.
          ;; FIXME: try to find a better way to do this.
          (.edge ^IGraph edge from-id to-id)
          ;; Don't cache the document in a mutable graph.
          (graph/->E label (graph/->V g from-id nil nil) (graph/->V g to-id nil nil) nil true nil))
        (catch IllegalArgumentException e
          nil))))

  p/AllVertices
  (all-vertices [g]
    (map #(graph/->V g (p/element-id %) nil nil)
      (p/all-vertices @(.ga g))))

  p/AllEdges
  (all-edges [g]
    (p/all-edges g (._getLabels ^IEdgeGraphs g)))
  (all-edges [g labels]
    (letfn [(next-edges [labels]
              (when (seq labels)
                (let [label (first labels)]
                  (if-let [^IGraph edge (._getEdgeGraph ^IEdgeGraphs g label)]
                    (lazy-seq
                      (concat
                        (map (fn [^IEdge e]
                               (graph/->E label
                                 (graph/->V g (.from e) nil nil)
                                 (graph/->V g (.to e) nil nil)
                                 nil true nil))
                          (iterator-seq (.iterator (.edges edge))))
                        (next-edges (rest labels))))
                    (next-edges (rest labels))))))]
      (next-edges labels)))

  p/GetVertex
  (get-vertex [g id]
    (graph/->V g id nil nil)))
