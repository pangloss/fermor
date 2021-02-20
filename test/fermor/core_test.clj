(ns fermor.traverse-test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [fermor.core :as g :refer :all :exclude [is]]
            [fermor.descend :refer [*no-result-interval* *cut-no-results* *no-results* cut-no-results value-for-no-results
                                    continue-no-results]]))

(deftest extrude-basic-descend-calls
  (testing "finite"
    (is (= [2]
           (descend [] (constantly []) [2])))
    (is (= [0 1 2 3 -1 0 1 2 3 -2]
           (descend [] (fn [path v] (when (zero? v) [1 2 3])) [0 -1 0 -2]))))
  (testing "stack overflow caused by concat in heavy left"
    (is (= [2 1 1 1 1]
           (take 5 (descend [] (constantly [1]) [2]))))
    (is (= [1 1 1]
           (take 3 (drop 500000 (descend [] (constantly [1]) [2]))))))
  (testing "stack overflow caused by concat in heavy right"
    (is (= [0 1 2 3 4]
           (take 5 (descend [] (constantly []) (range)))))
    (is (= [500000 500001 500002]
           (take 3 (drop 500000 (descend [] (constantly []) (range)))))))
  (testing "mixed tree"
    (is (= [0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2]
           (take 20 (descend [] (fn [path v] (range v)) (range)))))
    (is (= 18
           (apply max (take 500000 (descend [] (fn [path v] (range v)) (range))))))
    (is (= [0 1 "1:0" :3 2 "2:0" :3 "2:1" :3 3 "3:0" :3 "3:1" :3 "3:2" :3 4 "4:0" :3 "4:1"]
           (take 20 (descend [] (fn [path v]
                                  (cond (string? v)
                                        (when-not (re-find #"/" v)
                                          [(keyword (str (count v)))])
                                        (number? v)
                                        (map (fn [n] (str v ":" n))
                                             (range v))))
                             (range)))))))

(deftest descend-with-control
  (testing "loop"
    (is (= [] (descend [] (fn [path v] continue) (fn [path v] (when (= 0 v) [1 2 3])) [0 -1 0 -2])))

    (is (= [1 2 3 -1 1 2 3 -2]
           (descend []
                    (fn [path v]
                      (case (int v) 0 continue emit))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (is (= [1 2 3 -1 1 2 3 -2]
           (descend []
                    (fn [path v]
                      (case (int v) 0 continue emit-and-continue))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (is (= [0 -1 0 -2]
           (descend []
                    (fn [path v] emit)
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (is (= [0 1 -1 0 1 -2]
           (descend []
                    (fn [path v]
                      (case (int v) 2 cut emit-and-continue))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (is (= [0 1 2 -1 0 1 2 -2]
           (descend []
                    (fn [path v]
                      (case (int v) 2 emit-and-cut emit-and-continue))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (is (= [0 1 0 1]
           (descend []
                    (fn [path v]
                      (case (int v) 0 emit-and-continue 1 emit-and-continue 2 cut ignore))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (binding [*no-result-interval* 100
              *cut-no-results* 1000]
      (is (= (range 10)
             (take 200 (descend [] (fn [path v] (if (> 10 v) emit-and-continue ignore)) (fn [path v] []) (range))))))




    (binding [*no-results* (fn [chk-buf depth down right]
                             (cut-no-results))]
      (is (= (range 10)
             (take 20 (descend [] (fn [path v] (if (> 10 v) emit-and-continue ignore)) (fn [path v] []) (range))))))

    (binding [*no-results* (fn [chk-buf depth down right]
                             (value-for-no-results chk-buf :nothing down right))]
      (is (= [0 1 2 3 4 5 6 7 8 9 :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing]
             (take 20 (descend [] (fn [path v] (if (> 10 v) emit-and-continue ignore)) (fn [path v] []) (range))))))

    (binding [*no-results* (fn [chk-buf depth down right]
                             (value-for-no-results chk-buf :nothing nil nil))]
      (is (= [0 1 2 3 4 5 6 7 8 9 :nothing]
             (take 20 (descend [] (fn [path v] (if (> 10 v) emit-and-continue ignore)) (fn [path v] []) (range))))))

    (binding [*no-result-interval* 100
              *cut-no-results* 100]
      (is (= [100 100 100]
             (take 3 (descend []
                              (fn [path v] (if (= 100 v) emit-and-continue ignore))
                              (fn [path v] (range))
                              (range))))))

    (binding [*no-result-interval* 99
              *cut-no-results* 99]
      (is (= []
             (take 3 (descend []
                              (fn [path v] (if (= 100 v) emit-and-continue ignore))
                              (fn [path v] (range))
                              (range))))))

    (let [lazy-coll (binding [*no-result-interval* 100
                              *cut-no-results* 100]
                      (seq (descend []
                                    (fn [path v] (if (= 100 v) emit-and-continue ignore))
                                    (fn [path v] (range))
                                    (range))))]
      (binding [*no-result-interval* 99
                *cut-no-results* 99]
                                        ; The inner binding should be ignored when the seq is executed because
                                        ; it's already all baked into the seq when it was first resolved.
        (is (= [100 100 100] (take 3 lazy-coll)))))

    (binding [*no-result-interval* 10
              *no-results* (fn [b v down right]
                             (continue-no-results nil right))]
      (is (= 1110
             (count (descend []
                                        ; this control block is intentionally terrible to force worst case behaviours
                             (fn [path v]
                               (if (and (>= 2 (count path))
                                        (every? #(> 10 %) path))
                                 (if (> 10 v) emit-and-continue continue)
                                 ignore))
                                        ; infinite space in every direction
                             (fn [path v] (range))
                             (range))))))

    (is (= (take 100 (repeat 0))
           (take 100 (descend nil
                              (constantly emit-and-chain)
                              (constantly (range))
                              (range)))))))


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
    (is (= #{:a :b :c :x :y} (set (map element-id (vertices g)))))
    (is (= #{nil {:x 1}} (set (documents (out-e :normal (vertices g))))))
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
           (set (documents (out-e :p (vertices g))))))
    (is (= #{11.0 23.0}
           (set (documents (out-e :w (vertices g))))))))
