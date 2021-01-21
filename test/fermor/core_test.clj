(ns fermor.traverse-test
  (:require [clojure.test :as t :refer [deftest testing]]
            [fermor.traverse :as g]))

(deftest extrude-basic-descend-calls
  (testing "finite"
    (t/is (= [2]
           (g/descend [] (constantly []) [2])))
    (t/is (= [0 1 2 3 -1 0 1 2 3 -2]
           (g/descend [] (fn [path v] (when (zero? v) [1 2 3])) [0 -1 0 -2]))))
  (testing "stack overflow caused by concat in heavy left"
    (t/is (= [2 1 1 1 1]
           (take 5 (g/descend [] (constantly [1]) [2]))))
    (t/is (= [1 1 1]
           (take 3 (drop 500000 (g/descend [] (constantly [1]) [2]))))))
  (testing "stack overflow caused by concat in heavy right"
    (t/is (= [0 1 2 3 4]
           (take 5 (g/descend [] (constantly []) (range)))))
    (t/is (= [500000 500001 500002]
           (take 3 (drop 500000 (g/descend [] (constantly []) (range)))))))
  (testing "mixed tree"
    (t/is (= [0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4 0 1 0 2]
           (take 20 (g/descend [] (fn [path v] (range v)) (range)))))
    (t/is (= 18
           (apply max (take 500000 (g/descend [] (fn [path v] (range v)) (range))))))
    (t/is (= [0 1 "1:0" :3 2 "2:0" :3 "2:1" :3 3 "3:0" :3 "3:1" :3 "3:2" :3 4 "4:0" :3 "4:1"]
             (take 20 (g/descend [] (fn [path v]
                                      (cond (string? v)
                                            (when-not (re-find #"/" v)
                                              [(keyword (str (count v)))])
                                            (number? v)
                                            (map (fn [n] (str v ":" n))
                                                 (range v))))
                             (range)))))))

(deftest descend-with-control
  (testing "loop"
    (t/is (= [] (g/descend [] (fn [path v] g/continue) (fn [path v] (when (= 0 v) [1 2 3])) [0 -1 0 -2])))

    (t/is (= [1 2 3 -1 1 2 3 -2]
           (g/descend []
                    (fn [path v]
                      (case (int v) 0 g/continue g/emit))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (t/is (= [1 2 3 -1 1 2 3 -2]
           (g/descend []
                    (fn [path v]
                      (case (int v) 0 g/continue g/emit-and-continue))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (t/is (= [0 -1 0 -2]
           (g/descend []
                    (fn [path v] g/emit)
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (t/is (= [0 1 -1 0 1 -2]
           (g/descend []
                    (fn [path v]
                      (case (int v) 2 g/cut g/emit-and-continue))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (t/is (= [0 1 2 -1 0 1 2 -2]
           (g/descend []
                    (fn [path v]
                      (case (int v) 2 g/emit-and-cut g/emit-and-continue))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (t/is (= [0 1 0 1]
           (g/descend []
                    (fn [path v]
                      (case (int v) 0 g/emit-and-continue 1 g/emit-and-continue 2 g/cut g/ignore))
                    (fn [path v] (when (= 0 v) [1 2 3]))
                    [0 -1 0 -2])))

    (binding [g/*no-result-interval* 100
              g/*cut-no-results* 1000]
      (t/is (= (range 10)
               (take 200 (g/descend [] (fn [path v] (if (> 10 v) g/emit-and-continue g/ignore)) (fn [path v] []) (range))))))




    (binding [g/*no-results* (fn [chk-buf depth down right]
                               (g/cut-no-results))]
      (t/is (= (range 10)
               (take 20 (g/descend [] (fn [path v] (if (> 10 v) g/emit-and-continue g/ignore)) (fn [path v] []) (range))))))

    (binding [g/*no-results* (fn [chk-buf depth down right]
                               (g/value-for-no-results chk-buf :nothing down right))]
      (t/is (= [0 1 2 3 4 5 6 7 8 9 :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing :nothing]
               (take 20 (g/descend [] (fn [path v] (if (> 10 v) g/emit-and-continue g/ignore)) (fn [path v] []) (range))))))

    (binding [g/*no-results* (fn [chk-buf depth down right]
                               (g/value-for-no-results chk-buf :nothing nil nil))]
      (t/is (= [0 1 2 3 4 5 6 7 8 9 :nothing]
               (take 20 (g/descend [] (fn [path v] (if (> 10 v) g/emit-and-continue g/ignore)) (fn [path v] []) (range))))))

    (binding [g/*no-result-interval* 100
              g/*cut-no-results* 100]
      (t/is (= [100 100 100]
             (take 3 (g/descend []
                              (fn [path v] (if (= 100 v) g/emit-and-continue g/ignore))
                              (fn [path v] (range))
                              (range))))))

    (binding [g/*no-result-interval* 99
              g/*cut-no-results* 99]
      (t/is (= []
             (take 3 (g/descend []
                              (fn [path v] (if (= 100 v) g/emit-and-continue g/ignore))
                              (fn [path v] (range))
                              (range))))))

    (let [lazy-coll (binding [g/*no-result-interval* 100
                              g/*cut-no-results* 100]
                      (seq (g/descend []
                                    (fn [path v] (if (= 100 v) g/emit-and-continue g/ignore))
                                    (fn [path v] (range))
                                    (range))))]
      (binding [g/*no-result-interval* 99
                g/*cut-no-results* 99]
        ; The inner binding should be ignored when the seq is executed because
        ; it's already all baked into the seq when it was first resolved.
        (t/is (= [100 100 100] (take 3 lazy-coll)))))

    (binding [g/*no-result-interval* 10
              g/*no-results* (fn [b v down right]
                               (g/continue-no-results nil right))]
      (t/is (= 1110
             (count (g/descend []
                             ; this control block is intentionally terrible to force worst case behaviours
                             (fn [path v]
                               (if (and (>= 2 (count path))
                                        (every? #(> 10 %) path))
                                 (if (> 10 v) g/emit-and-continue g/continue)
                                 g/ignore))
                             ; infinite space in every direction
                             (fn [path v] (range))
                             (range))))))

    (t/is (= (take 100 (repeat 0))
           (take 100 (g/descend nil
                              (constantly g/emit-and-chain)
                              (constantly (range))
                              (range)))))))
