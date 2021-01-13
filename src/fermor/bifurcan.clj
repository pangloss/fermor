(ns fermor.bifurcan
  (:refer-clojure :exclude [filter])
  (:require [fermor.traverse :refer [join m filter]]
            [clojure.pprint :refer [simple-dispatch]])
  (:import (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph Graphs ISet)
           (clojure.lang IMeta)))

(defmethod print-method IGraph [o ^java.io.Writer w]
  (.write w "<Graph!>"))

(defmethod simple-dispatch IGraph [o]
  (print-method o *out*))

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


(defn dag
  ([] (DirectedAcyclicGraph.))
  ([linear?]
   (if linear?
     (.linear (dag))
     (dag))))
(defn digraph
  ([] (DirectedGraph.))
  ([linear?]
   (if linear?
     (.linear (digraph))
     (digraph))))
(defn undirected-graph
  ([] (io.lacuna.bifurcan.Graph.))
  ([linear?]
   (if linear?
     (.linear (undirected-graph))
     (undirected-graph))))

(defn -e [^IGraph g]
  (iterator-seq (.iterator (.edges g))))

(defn -v [^IGraph g]
  (.vertices g))

(defn e* [{:keys [:graph/edges] :as graph}]
  (map (fn [[label g]]
         (some-> (-e g) (with-meta {:graph graph :edge/label label})))
       edges))

(defn e [graph]
  (with-meta (join (e* graph)) {:graph graph}))

(defn ^:dynamic *no-graph-in-metadata* [r]
  (throw (Exception. "No graph present in metadata")))

(defn ^:dynamic *not-imeta* [meta x]
  (throw (Exception. "Can't attach metadata")))

(defn as-route [graph x]
  (with-meta [x] {:graph graph}))

(defn use-graph [graph x]
  (let [meta {:graph graph}]
    (if (instance? IMeta x)
      (with-meta x meta)
      (if (and x (seqable? x))
        (when-let [s (seq x)]
          (with-meta s meta))
        (*not-imeta* meta x)))))

(defn vertices-with-edge [graph label]
  (->> (get-in graph [:graph/edges label])
       -v
       (use-graph graph)))

(defn -out*
  ([^IGraph g ^ISet vs vr]
   (with-meta
     (map #(when (.isPresent (.indexOf vs %))
             (.out g %))
          vr)
     (meta vr)))
  ([^IGraph g ^ISet vs f vr]
   (with-meta
     (map #(f (when (.isPresent (.indexOf vs %))
                 (.out g %)))
          vr)
     (meta vr))))

(defn -in*
  ([^IGraph g ^ISet vs vr]
   (with-meta
     (map #(when (.isPresent (.indexOf vs %))
             (.in g %))
          vr)
     (meta vr)))
  ([^IGraph g ^ISet vs f vr]
   (with-meta
     (map #(f (when (.isPresent (.indexOf vs %))
                (.in g %)))
          vr)
     (meta vr))))

(defn graph-out*
  ([{:keys [:graph/edges] :as graph} label vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (let [vs (.vertices g)]
         (-out* g vs #(use-graph graph %) (use-graph graph vr))))))
  ([{:keys [:graph/edges] :as graph} label f vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (let [vs (.vertices g)]
         (-out* g vs (comp f #(use-graph graph %)) (use-graph graph vr)))))))

(defn graph-in*
  ([{:keys [:graph/edges] :as graph} label vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (let [vs (.vertices g)]
         (-in* g vs #(use-graph graph %) (use-graph graph vr))))))
  ([{:keys [:graph/edges] :as graph} label f vr]
   (when vr
     (when-let [^IGraph g (label edges)]
       (let [vs (.vertices g)]
         (-in* g vs (comp f #(use-graph graph %)) (use-graph graph vr)))))))

(defn graph-out
  ([{:keys [:graph/edges] :as graph} label vr]
   (join (graph-out* graph label vr)))
  ([{:keys [:graph/edges] :as graph} label f vr]
   (join (graph-out* graph label f vr))))

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

(defn in*
  ([label vr]
   (if-let [g (:graph (meta vr))]
     (graph-in* g label vr)
     (*no-graph-in-metadata* vr)))
  ([label f vr]
   (if-let [g (:graph (meta vr))]
     (graph-in* g label f vr)
     (*no-graph-in-metadata* vr))))

(defn out
  ([label vr]
   (join (out* label vr)))
  ([label f vr]
   (join (out* label f vr))))

(defn in
  ([label vr]
   (join (in* label vr)))
  ([label f vr]
   (join (in* label f vr))))

(defn in-sorted
  "Like in, but use sort-by-f to sort the elements attached to each vertex before including them in the overall collection."
  [labels sort-by-f r]
  (in labels #(sort-by sort-by-f %) r))

(defn out-sorted
  "Like out, but use sort-by-f to sort the elements attached to each vertex before including them in the overall collection."
  [labels sort-by-f r]
  (out labels #(sort-by sort-by-f %) r))

(defn has-property
  "Filters the route for elements that have the specified property."
  ([property r]
   (if-let [props (:graph/properties (:graph (meta r)))]
     (filter #(find (props %) property) r)
     (*no-graph-in-metadata* r)))
  ([property values-set r]
   (if-let [props (:graph/properties (:graph (meta r)))]
     (filter #(values-set (get (props %) property)) r)
     (*no-graph-in-metadata* r))))

(defn property
  "Produces a lazy sequence of the value of the given property."
  [property r]
  (if-let [props (:graph/properties (:graph (meta r)))]
    (keep #(get (props %) property) r)
    (*no-graph-in-metadata* r)))

(defn properties
  "Produces a lazy sequence of the value of the given property."
  [r]
  (if-let [props (:graph/properties (:graph (meta r)))]
    (keep #(props %) r)
    (*no-graph-in-metadata* r)))
