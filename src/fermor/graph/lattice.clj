(ns fermor.graph.lattice
  (:require [fermor.protocols :refer [get-vertex element-id vertex?]]
            [fermor.graph :as fg :refer [edge-graph]]
            [fermor.graph.algo :refer [breadth-first-reduce breadth-first-nodes]]
            [fermor.core :as g]))

(defrecord SymmetricLattice [label depths dual-depths top bottom up down])

(defn- max-path-length [rf v]
  (dec (apply max (map count (g/deepest-paths rf v)))))

(defn symmetric? [^SymmetricLattice lattice]
  (let [depth (.depths lattice)
        middle (/ (reduce max (vals depth)) 2)]
    (->> (.top lattice)
      (g/deepest-paths (.down lattice))
      (map (fn [path]
             (apply + (map (comp depth g/element-id) path))))
      (every? zero?))))

(defn symmetric-lattice [graph label]
  (let [vs (g/vertices-with-edge graph label)
        tops (remove #(seq (g/in-edges % [label])) vs)
        bottoms (remove #(seq (g/out-edges % [label])) vs)]
    (if (seq vs)
      (if (or (next tops) (next bottoms))
        (throw (ex-info "Invalid lattice. Must have only one top and one bottom"
                 {:graph graph :label label :tops tops :bottoms bottoms}))
        (let [ups (mapv (partial max-path-length #(g/in label %)) vs)
              downs (mapv (partial max-path-length #(g/out label %)) vs)
              height (reduce max ups)
              height (if (odd? height) (inc height) height) ;; make sure there is a center line
              middle (/ height 2)
              depths (reduce (fn [m [node top-dist bot-dist]]
                               (assoc m (g/element-id node)
                                 (cond (= top-dist bot-dist) 0
                                       (< bot-dist top-dist) (- bot-dist middle)
                                       :else (- height top-dist middle))))
                       {}
                       (map vector vs ups downs))
              dual-depths (reduce-kv (fn [d node depth]
                                       (assoc d node (- depth)))
                            {} depths)
              lattice (->SymmetricLattice
                        label
                        depths
                        dual-depths
                        (first tops)
                        (first bottoms)
                        #(g/in [label] %)
                        #(g/out [label] %))]
          (if (symmetric? lattice)
            lattice
            (throw (ex-info "Non-symmetric lattice." {:graph graph :label label})))))
      (throw (ex-info "Attempting to create an empty lattice" {:graph graph :label label})))))

(defn depth [^SymmetricLattice lattice v]
  ((.depths lattice) (g/element-id v)))

(defn dual [lattice]
  (->SymmetricLattice (.label lattice)
    (.dual-depths lattice) (.depths lattice)
    (.bottom lattice) (.top lattice)
    (.down lattice) (.up lattice)))

(defn meet [^SymmetricLattice lattice a b]
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

(defn join [^SymmetricLattice lattice a b]
  (meet (dual lattice) a b))


