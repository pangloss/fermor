(ns fermor.bifurcan
  (:import (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph Graphs)
           (clojure.lang IMeta)))

(defn forked [g]
  (if (instance? IGraph g)
    (.forked g)
    (-> g
        (assoc :graph/linear false)
        (update :graph/edges #(reduce (fn [r [label ^IGraph g]]
                                        (assoc r label (.forked g)))
                                      {} %)))))

(defn linear [g]
  (if (instance? IGraph g)
    (.linear g)
    (-> g
        (assoc :graph/linear true)
        (update :graph/edges #(reduce (fn [r [label ^IGraph g]]
                                        (assoc r label (.linear g)))
                                      {} %)))))

(defn edge-builder
  ([{:keys [:graph/linear] :as graph} edge-type f]
   (edge-builder graph edge-type (cond-> (DirectedGraph.) linear (.linear)) f))
  ([graph edge-type base-type f]
   (update-in graph [:graph/edges edge-type]
              #(f (or % base-type)))))

(defn dag [] (DirectedAcyclicGraph.))
(defn digraph [] (DirectedGraph.))
(defn undirected-graph [] (io.lacuna.bifurcan.Graph.))

(defn ^:dynamic *no-graph-in-metadata* [r]
  (throw (Exception. "No graph present in metadata")))

(defn use-graph [graph x]
  (when (instance? IMeta x)
    (with-meta x {:graph graph})))

(defn join [r]
  (if (instance? IMeta r)
    (with-meta (apply concat r) (meta r))
    (apply concat r)))

(defn -out+
  ([^IGraph g vr]
   (with-meta
     (map #(.out g %) vr)
     (meta vr)))
  ([^IGraph g f vr]
   (with-meta
     (map #(f (.out g %)) vr)
     (meta vr))))

(defn graph-out*
  ([{:keys [:graph/edges] :as graph} label vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (-out* g (use-graph graph vr)))))
  ([{:keys [:graph/edges] :as graph} label f vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (-out* g f (use-graph graph vr))))))

(defn graph-out
  ([{:keys [:graph/edges] :as graph} label vr]
   (join (graph-out* graph label vr)))
  ([{:keys [:graph/edges] :as graph} label f vr]
   (join (graph-out* graph label f vr))))

(defn graph-in*
  ([{:keys [:graph/edges] :as graph} label vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (-in* g (use-graph graph vr)))))
  ([{:keys [:graph/edges] :as graph} label f vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (-in* g f (use-graph graph vr))))))

(defn graph-in
  ([{:keys [:graph/edges] :as graph} label vr]
   (join (graph-in* graph label vr)))
  ([{:keys [:graph/edges] :as graph} label f vr]
   (join (graph-in* graph label f vr))))

(defn out*
  ([label vr]
   (if-let [g (:graph (meta vr))]
     (graph-out* g label vr)
     (*no-graph-in-metadata* vr)))
  ([label f vr]
   (if-let [g (:graph (meta vr))]
     (graph-out* g label f vr)
     (*no-graph-in-metadata* vr))))

(defn out
  ([x vr]
   (join (out* x vr)))
  ([g x vr]
   (join (out* g x vr))))

(defn -in*
  ([^IGraph g vr]
   (with-meta
     (map #(.in g %) vr)
     (meta vr)))
  ([^IGraph g f vr]
   (with-meta
     (map #(f (.in g %)) vr)
     (meta vr))))

(defn join [r]
  (if (instance? IMeta r)
    (with-meta (apply concat r) (meta r))
    (apply concat r)))

(defn in*
  ([label vr]
   (if-let [g (:graph (meta vr))]
     (graph-in* g label vr)
     (*no-graph-in-metadata* vr)))
  ([label f vr]
   (if-let [g (:graph (meta vr))]
     (graph-in* g label f vr)
     (*no-graph-in-metadata* vr))))

(defn in
  ([x vr]
   (join (in* x vr)))
  ([g x vr]
   (join (in* g x vr))))

(defn in-sorted
  "Like in, but use sort-by-f to sort the elements attached to each vertex before including them in the overall collection."
  [labels sort-by-f r]
  (in labels #(sort-by sort-by-f %) r))

(defn out-sorted
  "Like out, but use sort-by-f to sort the elements attached to each vertex before including them in the overall collection."
  [labels sort-by-f r]
  (out labels #(sort-by sort-by-f %) r))
