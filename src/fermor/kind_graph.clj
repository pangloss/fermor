(ns fermor.kind-graph
  (:use fermor.protocols)
  (:require [conditions :refer [condition error default optional]]
            [fermor.graph :refer [print-edge v e-> e<- ->V]]
            [clojure.pprint :refer [simple-dispatch]])
  (:import (fermor.protocols KindId)
           (java.util Optional)))

(declare ->KEdge)

(deftype KVertex [element]
  Object
  (equals [a b] (and (instance? KVertex b) (= element (.element ^KVertex b))))
  (hashCode [e] (.hashCode element))

  Element
  (element-id ^KindId [v] (element-id element))
  (get-graph [v] (get-graph element))
  (get-property [v key] (get-property element key))

  Kind
  (kind [v] (:kind (element-id v)))

  Wrappable
  (-unwrap [e] (-unwrap element))

  Vertex
  (-out-edges [v]
    (->> (-out-edges element)
         (map ->KEdge)))
  (-out-edges [v labels]
    (->> (-out-edges element labels)
         (map ->KEdge)))
  (-out-edges [v _ prepared-labels]
     (->> (-out-edges element _ prepared-labels)
          (map ->KEdge)))
  (-in-edges [v]
    (->> (-in-edges element)
         (map ->KEdge)))
  (-in-edges [v labels]
    (->> (-in-edges element labels)
         (map ->KEdge)))
  (-in-edges [v _ prepared-labels]
    (->> (-in-edges element _ prepared-labels)
         (map ->KEdge))))

(deftype KEdge [element]
  Object
  (equals [a b] (.equals element b))
  (hashCode [e] (.hashCode element))

  Element
  (element-id [e] (element-id element))
  (get-graph [e] (get-graph element))
  (get-property [e]
    (get-property element))

  Wrappable
  (-unwrap [e] (-unwrap element))

  Edge
  (-label [e] (-label element))
  (in-vertex [e] (KVertex. (in-vertex element)))
  (out-vertex [e] (KVertex. (out-vertex element))))

(deftype KGraph [graph]
  Object
  (equals [a b] (.equals graph b))
  (hashCode [e] (.hashCode graph))

  Graph
  (all-vertices [g]
    (map ->KVertex (all-vertices graph)))
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
    (when-let [p (get-property e)]
      (.write w " ")
      (print-method p w)))
  (.write w ")"))

(defmethod simple-dispatch KVertex [o]
  (print-method o *out*))

(defmethod print-method KEdge [^KEdge e ^java.io.Writer w]
  (print-edge "(E-> " "(E<- " (-unwrap e) w))

(defmethod simple-dispatch KEdge [o]
  (print-method o *out*))

(defn V
  ([kind id]
   (->KVertex (v kind id)))
  ([kind id property]
   (->KVertex (v kind id property))))

(defn E->
  ([out-id label in-id]
   (->KEdge (e-> out-id label in-id)))
  ([out-id label [property] in-id]
   (->KEdge (e-> out-id label [property] in-id))))

(defn E<-
  ([out-id label in-id]
   (->KEdge (e<- out-id label in-id)))
  ([out-id label [property] in-id]
   (->KEdge (e<- out-id label [property] in-id))))
