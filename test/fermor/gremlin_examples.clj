(ns fermor.gremlin-examples
  (:use fermor.core)
  (:require  [clojure.test :as t]))

;;; Examples from https://tinkerpop.apache.org/docs/current/recipes/

;; Degree centrality

(defn degree-centrality [g]
  (->> (all-vertices g)
       (map (fn [v] {:v v :degree (count (both-e v))}))
       (sort-by (comp - :degree))))

;; Betweeness Centrality
;;
;; Betweeness centrality is a measure of the number of times a vertex is found
;; between the shortest path of each vertex pair in a graph. Consider the
;; following graph for demonstration purposes:
;;
;; This algo is not large graph friendly.

(defn betweeness-centrality []
  ;; this doesn't get the same number as their example, but seems to correctly model their description.
  (let [g (-> (build-graph)
              (add-edges :next [[:a :b] [:b :c] [:b :d] [:c :e] [:d :e] [:e :f]])
              forked)]
    (->> (all-vertices g)
         (map with-path)
         (all (f->> both (remove cyclic-path?)))
         (map path)
         (group-by (juxt first last))
         vals
         (mapcat (f->> (group-by count) (into (sorted-map)) first val))
         (apply concat)
         (remove edge?)
         group-count)))

