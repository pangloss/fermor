(ns fermor.kind-graph
  (:use fermor.protocols)
  (:require [fermor.graph :refer [print-edge v e-> e->in ->V]]
            [clojure.pprint :refer [simple-dispatch]])
  (:import (fermor.protocols KindId)))

(set! *warn-on-reflection* true)

;; NOTE: the kind-graph was created before custom-graph and is fairly pointless
;; as a standalone graph with almost no delta from the base graph type. It
;; should probably be switched over to a simple custom graph instead, pending
;; performance testing of the custom graph.

(declare ->KEdge)

(deftype KVertex [element]
  Object
  (equals [a b] (and (instance? KVertex b) (= element (.element ^KVertex b))))
  (hashCode [e] (.hashCode element))

  Element
  (element-id ^KindId [v] (element-id element))
  (get-graph [v] (get-graph element))
  (exists? [v] (exists? element))

  GetDocument
  (get-document [v key] (get-document element key))

  Kind
  (kind [v] (:kind (element-id v)))

  Wrappable
  (-unwrap [e] (-unwrap element))

  VertexEdges
  (-out-edges [v]
    (->> (-out-edges element)
         (map ->KEdge)))
  (-out-edges [v labels]
    (->> (-out-edges element labels)
         (map ->KEdge)))
  (-in-edges [v]
    (->> (-in-edges element)
         (map ->KEdge)))
  (-in-edges [v labels]
    (->> (-in-edges element labels)
         (map ->KEdge)))

  VertexEdgesPrepared
  (-out-edges-prepared [v prepared-labels]
    (->> (-out-edges-prepared element prepared-labels)
         (map ->KEdge)))
  (-in-edges-prepared [v prepared-labels]
    (->> (-in-edges-prepared element prepared-labels)
         (map ->KEdge))))

(deftype KEdge [element]
  Object
  (equals [a b] (.equals element b))
  (hashCode [e] (.hashCode element))

  Element
  (element-id [e] (element-id element))
  (get-graph [e] (get-graph element))
  (exists? [e] (exists? element))

  GetDocument
  (get-document [e] (get-document element))

  Wrappable
  (-unwrap [e] (-unwrap element))

  Edge
  EdgeLabel
  (-label [e] (-label element))
  EdgeVertices
  (in-vertex [e] (KVertex. (in-vertex element)))
  (out-vertex [e] (KVertex. (out-vertex element))))

(defn- of-kind [kind-pred r]
  ;; copied from core
  (if (keyword? kind-pred)
    (filter #(= kind-pred (kind %)) r)
    (filter (comp kind-pred kind) r)))

(deftype KGraph [graph]
  Object
  (equals [a b] (.equals graph b))
  (hashCode [e] (.hashCode graph))

  Graph

  GraphSettings
  (-settings [g] (-settings graph))

  AllVertices
  (all-vertices [g]
    (map ->KVertex (all-vertices graph)))
  (all-vertices [g kind]
    (->> (all-vertices g)
         (of-kind kind)))

  AllEdges
  (all-edges [g]
    (map ->KEdge (all-edges graph)))
  (all-edges [g labels]
    (map ->KEdge (all-edges graph labels)))

  GetVertex
  (get-vertex [g kind-id]
    (->KVertex (->V (-unwrap g) kind-id nil nil)))
  (get-vertex [g kind id]
    (->KVertex (->V (-unwrap g) (k kind id) nil nil)))

  Wrappable
  (-unwrap [g]
    (-unwrap graph)))

(defmethod print-method KEdge [^KEdge e ^java.io.Writer w]
  (print-method (-unwrap e) w))

(defmethod simple-dispatch KEdge [o]
  (print-method o *out*))

(defmethod print-method KVertex [e ^java.io.Writer w]
  (.write w "(V ")
  (print-method (:kind (element-id e)) w)
  (.write w " ")
  (print-method (:id (element-id e)) w)
  (when-not *compact-vertex-printing*
    (when-let [p (get-document e)]
      (.write w " ")
      (print-method p w)))
  (.write w ")"))

(defmethod simple-dispatch KVertex [o]
  (print-method o *out*))

(defmethod print-method KEdge [^KEdge e ^java.io.Writer w]
  (print-edge "(E-> " "(E->in " (-unwrap e) w))

(defmethod simple-dispatch KEdge [o]
  (print-method o *out*))

(defn V
  ([kind id]
   (->KVertex (v (k kind id))))
  ([kind id document]
   (->KVertex (v (k kind id) document))))

(defn E->
  ([out-id label in-id]
   (->KEdge (e-> out-id label in-id)))
  ([out-id label [document] in-id]
   (->KEdge (e-> out-id label [document] in-id))))

(defn E->in
  ([out-id label in-id]
   (->KEdge (e->in out-id label in-id)))
  ([out-id label [document] in-id]
   (->KEdge (e->in out-id label [document] in-id))))
