(ns fermor.gremlin-examples
  (:require  [clojure.test :refer [deftest is testing]]
             [fermor.protocols :refer [*compact-path-printing*]]
             [fermor.core :refer :all :as c :exclude [is]]))

;;; Examples from https://tinkerpop.apache.org/docs/current/recipes/

;; Traversal Between Vertices

(def job-graph
  (-> (build-graph)
      (add-edges :completes [[:bob :appBob1]
                             [:bob :appBob2]
                             [:stephen :appStephen1]
                             [:stephen :appStephen2]])
      (add-edges :appliesTo [[:appBob1 :blueprintsJob1]
                             [:appBob2 :blueprintsJob2]
                             [:appStephen1 :rexsterJob1]
                             [:appStephen2 :blueprintsJob3]])
      (add-edges :created [[:blueprints :blueprintsJob1 {:creationDate "12/20/2015"}]
                           [:blueprints :blueprintsJob2 {:creationDate "12/15/2015"}]
                           [:blueprints :blueprintsJob3 {:creationDate "12/16/2015"}]
                           [:rexster :rexsterJob1 {:creationDate "12/18/2015"}]])
      (add-vertices [[:bob {:type :person :name "Bob"}]
                     [:stephen {:type :person :name "Stephen"}]
                     [:blueprints {:type :company :name "Blueprints, Inc"}]
                     [:rexster {:type :company :name "Rexster, LLC"}]])
      (forked)))

;; Even though this has a lot of indentation, it's much more comprehensible than
;; the Gremlin queries, where unclear scopes and magic operators abound.

(defn query*gremlin-port [person]
  ;; See `query` below for the nice version.
  (->> (all-vertices job-graph)
       (mapcat
        (fn [job]
          (->>
           job
           (in-e [:created])
           (mapcat
            (fn [created]
              (->>
               job
               (in [:appliesTo])
               (lookahead
                (f->> (in [:completes])
                      (c/is person)))
               (map (fn [application]
                      [job
                       (out-vertex created)
                       created
                       application]))))))))))

;; After a bit of staring at the above mess I realized it could be better
;; expressed using clojure's built-in `for` macro:

(defn query [person]
  (for [job (all-vertices job-graph)
        created (in-e [:created] job)
        application (->> job
                         (in [:appliesTo])
                         (lookahead (f->> (in [:completes])
                                          (c/is person))))]
    ;; no need to even name "company", though it could easily be if you prefer:
    [job (out-vertex created) created application]))

(deftest traverse-graph []
  (is (= [[(v :blueprintsJob3)
           (v :blueprints)
           (e->in :blueprints :created [{:creationDate "12/16/2015"}] :blueprintsJob3)
           (v :appStephen2)]
          [(v :rexsterJob1)
           (v :rexster)
           (e->in :rexster :created [{:creationDate "12/18/2015"}] :rexsterJob1)
           (v :appStephen1)]]
         (query (get-vertex job-graph :stephen))))

  (let [stephen (get-vertex job-graph :stephen)
        bob (get-vertex job-graph :bob)]
    ;; Show that the two queries are equivalent
    (is (= (mapcat query*gremlin-port [stephen bob])
           (mapcat query [stephen bob]))))) ;; => true

(comment
  (require '[criterium.core :refer [quick-bench]])
  (let [stephen (get-vertex job-graph :stephen)
        bob (get-vertex job-graph :bob)]
    (println "V1")
    (quick-bench (mapcat query*gremlin-port [stephen bob]))

    (println "V2") ;; Notice that this is also twice as fast
    (quick-bench (mapcat query [stephen bob]))
    ;; =>
    ;; V1
    ;; Evaluation count : 20718 in 6 samples of 3453 calls.
    ;;              Execution time mean : 29.169052 µs
    ;;     Execution time std-deviation : 1.459303 µs
    ;;    Execution time lower quantile : 28.024632 µs ( 2.5%)
    ;;    Execution time upper quantile : 31.584603 µs (97.5%)
    ;;                    Overhead used : 6.480276 ns
    ;;
    ;; Found 1 outliers in 6 samples (16.6667 %)
    ;; 	low-severe	 1 (16.6667 %)
    ;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
    ;; V2
    ;; Evaluation count : 44772 in 6 samples of 7462 calls.
    ;;              Execution time mean : 14.761135 µs
    ;;     Execution time std-deviation : 1.568145 µs
    ;;    Execution time lower quantile : 13.347101 µs ( 2.5%)
    ;;    Execution time upper quantile : 17.349297 µs (97.5%)
    ;;                    Overhead used : 6.480276 ns
    ;;
    ;; Found 1 outliers in 6 samples (16.6667 %)
    ;; 	low-severe	 1 (16.6667 %)
    ;;  Variance from outliers : 30.7695 % Variance is moderately inflated by outliers
    nil))


;; Degree centrality

(defn degree-centrality [g]
  (->> (all-vertices g)
       (map (fn [v] {:v v :degree (count (both-e v))}))
       (sort-by (comp - :degree))))

(deftest centrality
  (is (= (set [{:v (v :blueprints), :degree 3}
               {:v (v :bob), :degree 2}
               {:v (v :stephen), :degree 2}
               {:v (v :appStephen1), :degree 2}
               {:v (v :appBob1), :degree 2}
               {:v (v :appBob2), :degree 2}
               {:v (v :appStephen2), :degree 2}
               {:v (v :blueprintsJob3), :degree 2}
               {:v (v :blueprintsJob2), :degree 2}
               {:v (v :blueprintsJob1), :degree 2}
               {:v (v :rexsterJob1), :degree 2}
               {:v (v :rexster), :degree 1}])
         (set (degree-centrality job-graph)))))


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

(deftest betweeness-centrality
  ;; This doesn't get the same number as their example, but correctly models
  ;; their description. The gremlin code is too confusing to be confident in
  ;; understanding.
  (is (= {(v :a) 8
          (v :b) 14
          (v :c) 9
          (v :d) 9
          (v :e) 14
          (v :f) 8}
         (->> (all-vertices betweeness-graph)
              with-paths
              (all both)
              (map path)
              (group-by (juxt first last))
              vals
              (mapcat (f->> (group-by count) (into (sorted-map)) first val))
              (apply concat)
              (remove edge?)
              group-count))))

;; Cycle detection

(deftest find-all-cycles
  (is (= [[(v :b) (e-> :b :knows :c) (v :c) (e-> :c :knows :a) (v :a) (e-> :a :knows :b) (v :b)]
          [(v :c) (e-> :c :knows :a) (v :a) (e-> :a :knows :b) (v :b) (e-> :b :knows :c) (v :c)]
          [(v :a) (e-> :a :knows :b) (v :b) (e-> :b :knows :c) (v :c) (e-> :c :knows :a) (v :a)]]
         (->> (all-vertices cycles-graph)
              (map with-path)
              (all-cycles 3 out)
              (map path)))))

(comment
  ;; TODO: The basic graph does not allow duplicate edges, which breaks this
  ;; example. In practice duplicate edges are rare and cause problems so I
  ;; haven't decided whether to add support. If I do it will be via a
  ;; specialized graph wrapper that keeps an edge count and indexed subkeys for
  ;; the externally visable document in the edge document. It's a thin wrapperd
  ;; but I think helps no one.
  (def bridges-graph
    (-> (build-graph)
        ;; FIXME Can't build graph with duplicate edges.
        (add-edges :bridge [[:g :b] [:g :o] [:g :r]
                            [:o :b 1] [:o :b 2] [:o :r 1] [:o :r 2]])
        (forked)))

  (defn all-paths-over-bridges []
    (->> (all-vertices bridges-graph)
         (map with-path)
         out-e
         count
         (= 7))))

(defn- irange
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


(deftest recommend-stuff
  (is (= [[(V :person :jon)
           {2 [(V :product 3) (V :product 1) (V :product 5)],
            3 [(V :product 2) (V :product 4)]}]
          [(V :person :alice)
           {4 [(V :product 8) (V :product 10)],
            5 [(V :product 9) (V :product 2)],
            6 [(V :product 1)]}]
          [(V :person :bob)
           {2 [(V :product 6) (V :product 8) (V :product 10)],
            3 [(V :product 7) (V :product 9)]}]
          [(V :person :jill)
           {2 [(V :product 3) (V :product 1) (V :product 5)],
            3 [(V :product 7) (V :product 9)]}]
          [(V :person :jack)
           {2 [(V :product 6) (V :product 8) (V :product 10)],
            3 [(V :product 2) (V :product 4)]}]]
         (for [person (->> (all-vertices rec-graph :person))]
           [person (->> person
                        (out [:bought])
                        (with-set remove
                          (f->> (in :bought)
                                (not-id (k :person :alice))
                                (out :bought)
                                ;; At the end of the section, the Gremlin guide suggests adding randomness like this:
                                #_(random-sample 0.5)))
                        sorted-group-by-count)]))))
