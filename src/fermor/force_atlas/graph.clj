(ns fermor.force-atlas.graph
  (:require [fermor.core :as g]
            [fastmath.core :as fm :refer [atan2]]
            [fastmath.vector :as fv :refer [vec2 dist]]
            [untether.ugf :as ugf]
            [clojure.set :as set]
            [clojure.core.reducers :as r])
  (:import [fastmath.vector Vec2]))

(fm/use-primitive-operators)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn shapes [^long sides g]
  (g/descend []
      (fn [path e]
        (let [r (cond
                  ;; match
                  (and (= (count path) sides) (= e (first path))) g/emit-and-continue
                  ;; path too long
                  (< sides (count path)) g/ignore
                  ;; originate shapes on out-edges only. Prevents every shape being counted twice.
                  (and (= 1 (count path)) (g/followed-reverse? (second (g/path e)))) g/ignore
                  ;; duplicated element in path: invalid
                  (some #(= e %) path) g/ignore
                  ;; keep searching this path
                  :else g/continue)]
          r))
      (fn [path e] (g/both [:to] e))
      (g/with-paths (g/vertices g))))

(defn squares [g]
  ;; a pair of triangles that share an edge also look like a square, so to find only squares I have to
  ;; remove any squares that contain all of the elements of any triangle
  (let [tri (into #{}
              (map #(set (map g/element-id (remove g/edge? (g/path %)))))
              (shapes 3 g))]
    (->> (shapes 4 g)
      (remove #(tri (set (map g/element-id (remove g/edge? (g/subpath % 4))))))
      (map g/no-path)
      frequencies)))

(defn graph-from-triples [triples]
  (let [edges (map-indexed
                (fn [eid [from w to]]
                  [from to w])
                triples)
        g (-> (g/graph)
            (g/add-edges :to edges)
            g/forked)
        vc (count (g/vertices g [:to]))
        ec (count triples)]
    (with-meta g {:vc vc :ec ec})))

(deftype VDoc [id position velocity prev_velocity
               ^double size ^double mass ^long degree ^long squares])

(defn attach-vertex-documents [g]
  (let [sqs (squares g)]
    (g/forked
      (reduce (fn [lg v]
                (g/set-document lg v
                  (VDoc.
                     #_id (g/element-id v)
                     #_position (volatile! (vec2 (rand-int 100) (rand-int 100)))
                     #_velocity (volatile! (vec2 (rand) (rand)))
                     #_prev-velocity (volatile! (vec2 0 0))
                     #_size 1.0
                     #_mass 1.0
                     #_degree (g/degree v)
                     #_squares (sqs v 0))))
        (g/linear g)
        (g/vertices g)))))

(deftype EDoc [^double weight length angle])

(defn attach-edge-documents [g]
  (g/forked
    (reduce (fn [lg e]
              (g/set-document lg e
                (EDoc.
                  #_weight (g/get-document e)
                  #_length (volatile! 0)
                  #_angle (volatile! 0))))
      (g/linear g)
      (g/out-e [:to] (g/vertices g)))))

(defn update-edge-documents [g]
  (r/fold
    (fn ([] g) ([_ __] g))
    (fn [g e]
      (let [^VDoc from-doc (g/get-document (g/out-vertex e))
            ^VDoc to-doc (g/get-document (g/in-vertex e))
            fv @(.position from-doc)
            tv @(.position to-doc)
            ev (fv/sub fv tv)
            ^EDoc edoc (g/get-document e)]
        (vreset! (.length edoc) (fv/mag ev))
        (vreset! (.angle edoc) (fv/heading ev))
        g))
    (g/out-e [:to] (g/vertices g))))


(defn make-graph [triples]
  (->> triples
    graph-from-triples
    attach-vertex-documents
    attach-edge-documents
    update-edge-documents))

(defn vc ^long [g] (get (meta g) :vc))
(defn ec ^long [g] (get (meta g) :ec))

(def doc g/get-document)

(defn v-id [^VDoc doc] (.id doc))
(defn position! [^VDoc doc] (.position doc))
(defn position ^Vec2 [^VDoc doc] @(.position doc))
(defn v-x ^double [^VDoc doc] (.x (position doc)))
(defn v-y ^double [^VDoc doc] (.y (position doc)))
(defn velocity! [^VDoc doc] (.velocity doc))
(defn velocity ^Vec2 [^VDoc doc] @(.velocity doc))
(defn v-dx ^double [^VDoc doc] (.x (velocity doc)))
(defn v-dy ^double [^VDoc doc] (.y (velocity doc)))
(defn prev-velocity! [^VDoc doc] (.prev_velocity doc))
(defn prev-velocity ^Vec2 [^VDoc doc] @(.prev_velocity doc))
(defn v-mass ^double [^VDoc doc] (.mass doc))
(defn v-size ^double [^VDoc doc] (.size doc))
(defn v-degree ^long [^VDoc doc] (.degree doc))
(defn v-squares ^long [^VDoc doc] (.squares doc))

(defn e-weight ^double [^EDoc doc] (.weight doc))
(defn e-length ^double [^EDoc doc] @(.length doc))
(defn e-angle ^double [^EDoc doc] @(.angle doc))
