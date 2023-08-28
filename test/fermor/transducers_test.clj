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
          (into #{} (comp (out-e :normal) documents) (vertices g))))
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
          (into #{} (comp (out-e :p) documents) (vertices g))))
    (is (= #{11.0 23.0}
          (into #{} (comp (out-e :w) documents) (vertices g))))
    (is (= #{:b :c}
          (into #{} (comp (out :w) element-ids) (vertices g))))
    (is (= #{:a :b :c :x :y}
          (into #{} (comp (both) element-ids) (vertices g))))
    (is (= #{#{:y} #{:b} #{:x} #{:c :a}}
          (into #{} (comp (section set (comp (both) element-ids))) (vertices g))))
    (is (= #{#{:y} #{:b} #{:x} #{:c :a}}
          (into #{} (comp (section set (comp (both) element-ids))) (vertices g))))))

