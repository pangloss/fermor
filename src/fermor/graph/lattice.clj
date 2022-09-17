(ns fermor.graph.lattice
  (:require [fermor.protocols :refer [get-vertex element-id vertex?]]
            [fermor.graph :as fg :refer [edge-graph]]
            [fermor.graph.algo :refer [breadth-first-reduce breadth-first-nodes]]
            [fermor.core :as g]
            [clojure.walk :as walk]))

(defrecord Lattice [label depths dual-depths top bottom up down])

(defn max-path-length [rf v]
  (dec (apply max (map count (g/deepest-paths rf v)))))

(defn lattice [graph label]
  (let [vs (g/vertices-with-edge graph label)
        tops (remove #(seq (g/in-edges % [label])) vs)
        bottoms (remove #(seq (g/out-edges % [label])) vs)]
    (if (or (next tops) (next bottoms))
      (throw (ex-info "Invalid lattice. Must have only one top and one bottom" {:tops tops :bottoms bottoms}))
      (let [ups (mapv (partial max-path-length #(g/in label %)) vs)
            downs (mapv (partial max-path-length #(g/out label %)) vs)
            height (reduce max ups)
            height (if (odd? height) (inc height) height) ;; make sure there is a center line
            middle (/ height 2)
            depths (reduce (fn [m [node top-dist bot-dist]]
                             (assoc m (g/element-id node)
                               (cond (= top-dist bot-dist) middle
                                     (< bot-dist top-dist) bot-dist
                                     :else (- height top-dist))))
                     {}
                     (map vector vs ups downs))
            dual-depths (reduce-kv (fn [d node depth]
                                     (assoc d node (- height depth)))
                          {} depths)]
        (->Lattice
          label
          depths
          dual-depths
          (first tops)
          (first bottoms)
          #(g/in [label] %)
          #(g/out [label] %))))))


(defn depth [lattice v]
  (get-in lattice [:depths (g/element-id v)]))

(defn dual [lattice]
  (->Lattice (.label lattice)
    (.dual-depths lattice) (.depths lattice)
    (.bottom lattice) (.top lattice)
    (.down lattice) (.up lattice)))

(defn meet [^Lattice lattice a b]
  (cond (= (.top lattice) a) b
        (= (.top lattice) b) a
        (= (.bottom lattice) a) (.bottom lattice)
        (= (.bottom lattice) b) (.bottom lattice)
        (= a b) a
        :else
        (let [da (depth lattice a)
              db (depth lattice b)
              [lower higher] (if (< da db) [a b] [b a])
              ;; I could make this lazy as well through mutual recursion.
              ;; If the vector being queried is lower than the current one,
              ;; search down from the other side. But the tree isn't that big
              ;; and it hardly seems worth it. If performance of this is really
              ;; an issue, I think the entire set of meet/join combinations can
              ;; be pre-computed (except for the constants).
              meet? (into #{} (g/all (.down lattice) lower))]
          (breadth-first-reduce #(when (meet? %2) (reduced %2))
            nil (.down lattice) higher))))

(defn join [^Lattice lattice a b]
  (meet (dual lattice) a b))


(comment
  (def g (g/forked
           (g/add-edges (g/graph) :x
             '[[top a]
               [top b]
               [a a']
               [a' bottom]
               [b bottom]])))

  (def l (lattice g :x))

  [l (dual l)]

  (meet l
    (g/get-vertex g 'a)
    (g/get-vertex g 'a'))


  (join l
    (g/get-vertex g 'a)
    (g/get-vertex g 'a'))

  ,)
