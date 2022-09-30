(ns fermor.custom-graph-test
  (:require [fermor.custom-graph :as custom]
            [fermor.core :refer :all :exclude [is]]
            [fermor.protocols :as p]
            [clojure.test :refer [deftest is testing]]
            [fermor.custom-graph-test.impl :refer [->VExtended ->EExtended ->VRegion region]]))

(deftest different-weight-impl
  (let [orig-g (-> (graph)
                   (add-edges :xy [[(k :x :a) (k :y :b) {:weight 12}]
                                   [(k :x :a) (k :x :c)]])
                   forked)
        g (custom/wrap-graph orig-g nil nil nil ->VExtended ->EExtended)]
    (is (= #{:x :y} (set (->> (all-vertices g) (map (fn [x] (prn x (type x)) (kind x)))))))
    (is (= #{(k :x :a) (k :y :b) (k :x :c)} (set (->> (all-vertices g) (map element-id)))))
    (is (= #{12 nil}
           (set (->> (all-vertices g)
                     out-e
                     (map p/-weight)))))))

(deftest incorporate-any-interface
  (let [orig-g (forked (-> (graph)
                         (add-edges :region
                           [[:a :zone1 {:weight 12}]
                            [:b :zone1]
                            [:c :zone2]
                            [:zone2 :zone1]])))
        g (custom/wrap-graph orig-g nil nil nil ->VRegion nil)]
    (is (= #{:a :b :c :zone1 :zone2} (set (->> (all-vertices g) (map element-id)))))
    (is (= #{:a :b :c :zone1 :zone2} (set (->> (all-vertices (linear g)) (map element-id)))))
    (is (= #{:a :b :c :zone1 :zone2} (set (->> (all-vertices (forked (linear g))) (map element-id)))))
    (is (forked? g))
    (is (forked? (forked g)))
    (is (linear? (linear g)))
    (is (forked? (forked (linear g))))
    (is (= #{[(v :c) (v :zone2)]
             [(v :zone1) nil]
             [(v :b) (v :zone1)]
             [(v :zone2) (v :zone1)]
             [(v :a) (v :zone1)]}
          (->> (all-vertices g)
            (make-pairs region)
            set)))
    (is (= [(v :zone1)]
          (->> (all-vertices g)
            (keep region)
            (keep region))))
    (is (= #{(e-> :zone2 :region :zone1)
             (e-> :b :region :zone1)
             (e-> :c :region :zone2)
             (e-> :a :region [{:weight 12}] :zone1)}
          (set (all-edges g))))
    (is (= #{(e-> :zone2 :region :zone1)
             (e-> :b :region :zone1)
             (e-> :c :region :zone2)
             (e-> :a :region [{:weight 12}] :zone1)}
          (set (all-edges (linear g)))))))
