(ns fermor.build
  (:require [fermor.protocols :as p]
            [fermor.graph :as graph])
  (:import (fermor.graph IEdgeGraphs IGraphData)
           (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph IEdge)))

(defprotocol ResetGraph
  (-reset-graph [builder g]
    "Change the underlying graph for a builder. Use with caution as this will be a bit confusing."))

(def ^:private PResetGraph (:on-interface ResetGraph))

(declare ->BuilderGraph)

(defn builder? [g]
  (and (some? g) (.isAssignableFrom PResetGraph (class g))))

(defn builder
  "Create a new Builder Graph from an existing graph."
  ([graph]
   (builder (atom nil) graph))
  ([ga graph]
   (let [graph (p/to-forked graph)]
     (assert (not (builder? graph)))
     (reset! ga graph)
     (->BuilderGraph ga))))

(defmacro ^:private mutate [sym op]
  ;; This is a very nasty little macro that's just to save some repetition.
  ;; It requires the self argument to be called g and needs to be within the
  ;; implementation so that the ga atom is visible.
  `(if (p/linear? @~'ga)
     (let [~sym @~'ga]
       ~op
       ~'g)
     (let [~sym (p/to-linear @~'ga)]
       (assert (not (builder? ~sym)))
       (try
         (reset! ~'ga ~sym)
         ~op
         (finally
           (let [g# (p/to-forked ~sym)]
             (reset! ~'ga g#))))
       ~'g)))

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

  ;; we're both linear and forked :D
  p/Linear
  p/Forked

  p/ToLinear
  (to-linear [g]
    (p/to-linear @ga))

  p/ToForked
  (to-forked [g]
    (p/to-forked @ga))

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

  IGraphData
  (_getDocuments ^IMap [g]
    (._getDocuments @ga))
  (_getEdges ^IMap [g]
    (._getEdges @ga))

  p/GraphTranspose
  (-transpose [g]
    (p/-transpose g (._getLabels g)))
  (-transpose [g labels]
    ;; NOTE: unlike other functions, this one returns a new independent graph.
    (assert false "not impl")
    #_
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
      (p/add-vertices lg id-document-pairs)))

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
          (graph/->E label
            (-> (graph/->V g from-id nil nil) (with-meta {:ga ga}))
            (-> (graph/->V g to-id nil nil) (with-meta {:ga ga}))
            nil true nil))
        (catch IllegalArgumentException e
          nil))))

  p/AllVertices
  (all-vertices [g]
    (map #(-> (graph/->V g (p/element-id %) nil nil) (with-meta {:ga ga}))
      (p/all-vertices @ga)))

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
                                 (-> (graph/->V g (.from e) nil nil) (with-meta {:ga ga}))
                                 (-> (graph/->V g (.to e) nil nil) (with-meta {:ga ga}))
                                 nil true nil))
                          (iterator-seq (.iterator (.edges edge))))
                        (next-edges (rest labels))))
                    (next-edges (rest labels))))))]
      (next-edges labels)))

  p/GetVertex
  (get-vertex [g id]
    (-> (graph/->V g id nil nil) (with-meta {:ga ga}))))
