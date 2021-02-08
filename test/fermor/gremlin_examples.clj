(ns fermor.gremlin-examples
  (:use fermor.core)
  (:require  [clojure.test :as t]))

;;; Examples from https://tinkerpop.apache.org/docs/current/recipes/

;; Traversal Between Vertices

(def job-graph
  (-> (build-graph)
      (add-edges :completes [[:bob :appBob1] [:bob :appBob2]
                             [:stephen :appStephen1] [:stephen :appStephen2]])
      (add-edges :appliesTo [[:appBob1 :blueprintsJob1] [:appBob2 :blueprintsJob2]
                             [:appStephen1 :rexsterJob1] [:appStephen2 :blueprintsJob3]])
      (add-edges :created [[:blueprints :blueprintsJob1 {:creationDate "12/20/2015"}]
                           [:blueprints :blueprintsJob2 {:creationDate "12/15/2015"}]
                           [:blueprints :blueprintsJob3 {:creationDate "12/16/2015"}]
                           [:rexster :rexsterJob1 {:creationDate "12/18/2015"}]])
      (add-vertices [[:bob {:type :person :name "Bob"}]
                     [:stephen {:type :person :name "Stephen"}]
                     [:blueprints {:type :company :name "Blueprints, Inc"}]
                     [:rexster {:type :company :name "Rexster, LLC"}]])
      (forked)))

;; Something like the path object but with assignable names could do this perhaps a bit more elegantly?
;; Even though this has a lot of indentation, it's much more comprehensible than
;; the gremlin queries where unclear scopes and magic operators abound.


;; try with context

(let [stephen (get-vertex job-graph :stephen)
      bob (get-vertex job-graph :bob)
      query (fn [person]
              (->> (all-vertices job-graph)
                   (mapcat (fn [job]
                             (->> job
                                  (in-e [:created])
                                  (mapcat (fn [created]
                                            (let [company (out-vertex created)]
                                              (->> job
                                                   (in [:appliesTo])
                                                   (lookahead (f->> (in [:completes])
                                                                    (is person)))
                                                   (map (fn [application]
                                                          [job company created application])))))))))))]
  [(query stephen)
   (query bob)])

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

(def betweeness-graph
  (-> (build-graph)
      (add-edges :next [[:a :b] [:b :c] [:b :d] [:c :e] [:d :e] [:e :f]])
      forked))

(def cycles-graph
  (-> (build-graph)
      (add-edges :knows [[:a :b] [:b :c] [:c :a] [:a :d] [:c :d]])
      (forked)))

(defn betweeness-centrality []
  ;; this doesn't get the same number as their example, but seems to correctly model their description.
  (->> (all-vertices betweeness-graph)
       (map with-path)
       (all (f->> both (remove cyclic-path?)))
       (map path)
       (group-by (juxt first last))
       vals
       (mapcat (f->> (group-by count) (into (sorted-map)) first val))
       (apply concat)
       (remove edge?)
       group-count))


;; Cycle detection

(->> (all-vertices cycles-graph)
     (map with-path)
     (all-cycles 3 out))

(def bridges-graph
  (-> (build-graph)
      ;; FIXME Can't build graph with duplicate edges.
      (add-edges :bridge [[:g :b] [:g :o] [:g :r]
                          [:o :b 1] [:o :b 2] [:o :r 1] [:o :r 2]])
      (forked)))

(->> (all-vertices bridges-graph)
     (map with-path)
     out-e)

(defn irange
  "Helper to match the examples which are written in Groovy and have inclusive indexing on ranges."
  ([to] (range 1 (inc to)))
  ([from to] (range from (inc to)))
  ([from to step] (range from (inc to) step)))


;; Recommendation

(def rec-graph
  (let [people (reduce (fn [m id] (assoc m id (k :person id))) {} [:alice :bob :jon :jack :jill])
        products (into [nil] (mapv #(k :product %) (irange 10)))]
    (-> (build-graph)
        (add-edges :bought
                   (concat (for [i (irange 3 7)]
                             [(people :alice) (k :product i)])
                           (for [i (irange 5)]
                             [(people :bob) (k :product i)])
                           (for [i (irange 6 10)]
                             [(people :jon) (products i)])
                           (for [i (irange 1 10 2)]
                             [(people :jack) (products i)])
                           (for [i (irange 2 10 2)]
                             [(people :jill) (products i)])))
        forked
        fermor.kind-graph/->KGraph)))


(->> rec-graph all-vertices #_(filter #(= (k :person :alice) (element-id %)))
     (of-kind :person)
     (map (fn [person]
            [person
             (->> person
                  (out [:bought])
                  (into-set (fn [s r]
                              (->> r
                                   (in [:bought])
                                   (not-id (k :person :alice))
                                   (out [:bought])
                                   (random-sample 0.5) ;; at the end it suggests adding this
                                   (none-of s))))
                  sorted-group-by-count)])))
