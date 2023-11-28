(ns fermor.transducers-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [fermor.transducers :as g :refer :all :exclude [is]]
            [fermor.descend :refer [*no-result-interval* *cut-no-results* *no-results* cut-no-results value-for-no-results
                                    continue-no-results]]))

(deftest edges-are-eq
  (let [g (-> (graph)
              (add-edges :loom [[:a :b 4]])
              (add-edges :xyz [[:c :d]])
              (add-edges :nope [[:d :b]])
              (add-vertices [[:a {:info "ok!"}]])
              forked)]
    (is (= [(e->in :a :loom [4] :b)] [(e-> :a :loom [4] :b)]))
    (is (= [(e-> :a :loom [4] :b)] [(e->in :a :loom [4] :b)]))))



(deftest edge-flavours
  (let [g (-> (build-graph {:edge-builder {:w (fermor.graph/add-unique-weighted-edge 0.0)
                                           :p fermor.graph/add-parallel-edge}})
              (add-edges :normal [[:a :b {:k 9}]
                                  [:a :b {:x 1}]
                                  [:b :c {:bc :ski}]
                                  [:b :c]])
              (add-edges :p [[:a :b {:x 10}]
                             [:b :c {:x 11}]
                             [:b :c {:x 11}]
                             [:a :b {:x 2}]
                             [:a :b]
                             [:x :y]
                             [:a :b {:x 3}]
                             [:a :b {:x 4}]
                             [:a :b {:k 2}]
                             [:a :b {:k 9}]])
              (add-edges :w [[:a :b 10]
                             [:b :c 11]
                             [:a :b 2]
                             [:a :b 2]
                             [:a :b 2]
                             [:a :b 2]
                             [:a :b 5]])
              forked)]
    (is (= #{:a :b :c :x :y} (into #{} (map element-id) (vertices g))))
    (is (= #{nil {:x 1}}
          (into #{} (comp (out-e :normal) (documents)) (vertices g))))
    (is (= #{{:parallel/count 7
              0 {:x 10}
              1 {:x 2}
              2 nil
              3 {:x 3}
              4 {:x 4}
              5 {:k 2}
              6 {:k 9}}
             {:parallel/count 1 0 nil}
             {:parallel/count 2
              0 {:x 11}
              1 {:x 11}}}
          (into #{} (comp (out-e :p) (documents)) (vertices g))))
    (is (= #{11.0 23.0}
          (into #{} (comp (out-e :w) (documents)) (vertices g))))
    (is (= #{:b :c}
          (into #{} (comp (out :w) (element-ids)) (vertices g))))
    (is (= #{:a :b :c :x :y}
          (into #{} (comp (both) (element-ids)) (vertices g))))

    (is (= #{#{:y} #{:b} #{:x} #{:c :a}}
          (into #{} (comp (section #(apply set %)
                            (comp
                              (both*)
                              (map* (element-ids)))))
            (vertices g))))
    (is (= #{#{:y} #{:b} #{:x} #{:c :a}}
          (into #{} (comp (section set (comp (both) (element-ids)))) (vertices g))))
    (is (= #{#{:y} #{:b} #{:x} #{:c :a}}
          (into #{} (comp (section set (comp (both) (element-ids)))) (vertices g))))))



(def g (-> (build-graph {:edge-builder {:w (fermor.graph/add-unique-weighted-edge 0.0)
                                        :p fermor.graph/add-parallel-edge}})
         (add-edges :normal [[:a :b {:k 9}]
                             [:a :b {:x 1}]
                             [:b :c {:bc :ski}]
                             [:b :c]])
         (add-edges :p [[:a :b {:x 10}]
                        [:b :c {:x 11}]
                        [:b :c {:x 11}]
                        [:a :b {:x 2}]
                        [:a :b]
                        [:x :y]
                        [:a :b {:x 3}]
                        [:a :b {:x 4}]
                        [:a :b {:k 2}]
                        [:a :b {:k 9}]])
         (add-edges :w [[:a :b 10]
                        [:b :c 11]
                        [:a :b 2]
                        [:a :b 2]
                        [:a :b 2]
                        [:a :b 2]
                        [:a :b 5]])
         forked))

(deftest extrude-basic-descend-calls
  (testing "finite"
    (is (= [2]
          (descend [] (mapcat (constantly [])) [2])))
    (is (= [0 1 2 3 -1 0 1 2 3 -2]
          (descend [] (drop-path (mapcat (fn [v] (when (zero? v) [1 2 3])))) [0 -1 0 -2]))))
  (testing "stack overflow caused by concat in heavy left"
    (is (= [2 1 1 1 1]
          (take 5 (descend [] (drop-path (mapcat (constantly [1]))) [2]))))
    (is (= [1 1 1]
          (take 3 (drop 500000 (descend [] (map (constantly 1)) [2]))))))
  (testing "stack overflow caused by concat in heavy right"
    (is (= [0 1 2 3 4]
          (take 5 (descend [] (mapcat (constantly [])) (range)))))
    (is (= [500000 500001 500002]
          (take 3 (drop 500000 (descend [] (mapcat (constantly [])) (range)))))))
  (testing "mixed tree"
    (is (= [0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2]
          (take 20 (descend [] (drop-path (mapcat range)) (range)))))
    (is (= 18
          (apply max (take 500000 (descend [] (drop-path (mapcat range)) (range))))))
    (is (= [0 1 "1:0" :3 2 "2:0" :3 "2:1" :3 3 "3:0" :3 "3:1" :3 "3:2" :3 4 "4:0" :3 "4:1"]
          (take 20 (descend []
                     (drop-path
                       (cond-branch
                         string? (map (fn [v] (keyword (str (count v)))))
                         number? (mapcat (fn [v] (map (fn [n] (str v ":" n))
                                                  (range v))))))
                     (range)))))))

(defn alot [x]
  ;; NOTE: unlike the purely lazy version, this does not handle infinite sequences because
  ;; the built-in cat transducer is greedy. Fortunately I think infinite pathways in
  ;; a graph are not infinite in a single step so I hope this will not have an impact
  ;; in practice.
  (range 100000))

(deftest descend-with-control
  (testing "loop"
    (is (= [] (descend [] (fn [path v] continue) (mapcat (fn [v] (when (= 0 v) [1 2 3]))) [0 -1 0 -2])))

    (is (= [1 2 3 -1 1 2 3 -2]
          (descend []
            (fn [path v]
              (case (int v) 0 continue emit))
            (unwrapping-path (mapcat (fn [v] (when (= 0 v) [1 2 3]))))
            [0 -1 0 -2])))

    (is (= [1 2 3 -1 1 2 3 -2]
          (descend []
            (fn [path v]
              (case (int v) 0 continue emit-and-continue))
            (unwrapping-path (mapcat (fn [v] (when (= 0 v) [1 2 3]))))
            [0 -1 0 -2])))

    (is (= [0 -1 0 -2]
          (descend []
            (fn [path v] emit)
            (unwrapping-path (mapcat (fn [v] (when (= 0 v) [1 2 3]))))
            [0 -1 0 -2])))

    (is (= [0 1 -1 0 1 -2]
          (descend []
            (fn [path v]
              (case (int v) 2 cut emit-and-continue))
            (unwrapping-path (mapcat (fn [v] (when (= 0 v) [1 2 3]))))
            [0 -1 0 -2])))

    (is (= [0 1 2 -1 0 1 2 -2]
          (descend []
            (fn [path v]
              (case (int v) 2 emit-and-cut emit-and-continue))
            (unwrapping-path (mapcat (fn [v] (when (= 0 v) [1 2 3]))))
            [0 -1 0 -2])))

    (is (= [0 1 0 1]
          (descend []
            (fn [path v]
              (case (int v) 0 emit-and-continue 1 emit-and-continue 2 cut ignore))
            (unwrapping-path (mapcat (fn [v] (when (= 0 v) [1 2 3]))))
            [0 -1 0 -2])))

    (binding [*no-result-interval* 100
              *cut-no-results* 1000]
      (is (= (range 10)
            (take 200 (descend []
                        (fn [path v] (if (> 10 v) emit-and-continue ignore))
                        (mapcat (constantly []))
                        (range))))))




    (binding [*no-results* (fn [chk-buf depth down right]
                             (cut-no-results))]
      (is (= (range 10)
            (take 20 (descend []
                       (fn [path v] (if (> 10 v) emit-and-continue ignore))
                       (mapcat (constantly []))
                       (range))))))

    (binding [*no-results* (fn [chk-buf depth down right]
                             (value-for-no-results chk-buf :nothing down right))]
      (is (= [0 1 2 3 4 5 6 7 8 9 :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing]
            (take 20 (descend []
                       (fn [path v] (if (> 10 v) emit-and-continue ignore))
                       (mapcat (constantly []))
                       (range))))))

    (binding [*no-results* (fn [chk-buf depth down right]
                             (value-for-no-results chk-buf :nothing nil nil))]
      (is (= [0 1 2 3 4 5 6 7 8 9 :nothing]
            (take 20 (descend []
                       (fn [path v] (if (> 10 v) emit-and-continue ignore))
                       (mapcat (constantly []))
                       (range))))))

    (binding [*no-result-interval* 100
              *cut-no-results* 100]
      (is (= [100 100 100]
            (take 3 (descend []
                      (fn [path v] (if (= 100 v) emit-and-continue ignore))
                      (mapcat alot)
                      (range))))))

    (binding [*no-result-interval* 99
              *cut-no-results* 99]
      (is (= []
            (take 3 (descend []
                      (fn [path v] (if (= 100 v) emit-and-continue ignore))
                      (mapcat alot)
                      (range))))))

    (let [lazy-coll (binding [*no-result-interval* 100
                              *cut-no-results* 100]
                      (seq (descend []
                             (fn [path v] (if (= 100 v) emit-and-continue ignore))
                             (mapcat alot)
                             (range))))]
      (binding [*no-result-interval* 99
                *cut-no-results* 99]
        ;; The inner binding should be ignored when the seq is executed because
        ;; it's already all baked into the seq when it was first resolved.
        (is (= [100 100 100] (take 3 lazy-coll)))))

    (binding [*no-result-interval* 10
              *no-results* (fn [b v down right]
                             (continue-no-results nil right))]
      (is (= 1110
            (count (descend []
                     ;; this control block is intentionally terrible to force worst case behaviours
                     (fn [path v]
                       (if (and (>= 2 (count path))
                             (every? #(> 10 %) path))
                         (if (> 10 v) emit-and-continue continue)
                         ignore))
                     ;; infinite space in every direction
                     (mapcat alot)
                     (range))))))

    (is (= (take 10 (repeat 0))
          (take 10 (descend nil
                     (constantly emit-and-chain)
                     (mapcat alot)
                     (range)))))))


(comment
  (take 200 (chunked
              (mapcat (fn [x] (range 1000)))
              (range 5)))

  (chunk-buffer 10)


  (take 11 (all (mapcat (fn [x] [[x 1] [x 2] [x 3]])) (range 3)))

  (deepest 3 (mapcat (fn [x]
                       (if (= 4 x)
                         []
                         (range x))))
    (range 8))

  all

  nil

  (time
    (count
      (take 1000 (descend []
                   (when-path! (fn [p _] (> 2 (count p)))
                     (mapcat (fn [_](range 10))))
                   [2])))))
