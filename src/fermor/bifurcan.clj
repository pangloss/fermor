(ns fermor.bifurcan
  (:refer-clojure :exclude [filter keep])
  (:require [fermor.traverse :refer [join m filter keep]]
            [conditions :refer [condition error default]]
            [clojure.pprint :refer [simple-dispatch]])
  (:import (io.lacuna.bifurcan DirectedGraph DirectedAcyclicGraph IGraph Graphs ISet)
           (clojure.lang IMeta)))

(defn route-graph [r]
  (or (:graph (meta r))
      (condition :route/graph r (error "No graph present in route metadata"))))

(defn element-graph [r]
  (or (:graph (meta r))
      (condition :element/graph r (error "No graph present in element metadata"))))

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
  (some-> (join (e* graph)) (with-meta {:graph graph})))

(defn as-route [graph x]
  (with-meta [x] {:graph graph}))

(defn use-graph [graph x]
  (let [meta {:graph graph}]
    (if (instance? IMeta x)
      (with-meta x meta)
      (if x
        (if (seqable? x)
          (with-meta (or (seq x) []) meta)
          (condition :not-imeta [x meta] (error "Can't attach metadata to entities of this type" {:type (type x)})))
        (condition :not-imeta/nil meta (default (with-meta [] meta)))))))

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
   (let [g (route-graph vr)]
     (graph-out* g label vr)))
  ([label f vr]
   (let [g (route-graph vr)]
     (graph-out* g label f vr))))

(defn in*
  ([label vr]
   (let [g (route-graph vr)]
     (graph-in* g label vr)))
  ([label f vr]
   (let [g (route-graph vr)]
     (graph-in* g label f vr))))

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
   (let [props (:graph/properties (route-graph r))]
     (filter #(find (props %) property) r)))
  ([property values-set r]
   (let [props (:graph/properties (route-graph r))]
     (filter #(values-set (get (props %) property)) r))))

(defn property
  "Produces a lazy sequence of the value of the given property."
  [property r]
  (let [props (:graph/properties (route-graph r))]
    (keep #(get (props %) property) r)))

(defn properties
  "Produces a lazy sequence of the value of the given property."
  [r]
  (let [props (:graph/properties (route-graph r))]
    (keep #(props %) r)))
