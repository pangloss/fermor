(ns fermor.loom-test
  (:require [clojure.test :refer [deftest testing is are]]
            [fermor.core :refer [graph forked add-edges add-vertices v e-> e<-]]
            [fermor.loom-graph :refer [as-loom-graph]]
            [loom.graph :as loom]
            loom.attr))


(deftest loom-protocols
  (let [g (as-loom-graph (-> (fermor.core/graph)
                            (add-edges :loom [[:a :b 4]])
                            (add-edges :xyz [[:c :d]])
                            (add-edges :nope [[:d :b]])
                            (add-vertices [[:a {:info "ok!"}]])
                            forked)
                        {:edge-labels [:loom :xyz]
                         :weight/nil 9
                         :weight/no-edge 33})]
    (is (= [:b] (loom/successors* g :a)))
    (is (= 1 (loom/out-degree g :a)))

    (is (= [(e-> :a :loom [4] :b)] (loom/out-edges g :a)))

    (is (= [:a] (loom/predecessors* g :b)))
    (is (= 1 (loom/in-degree g :b)))

    (loom/in-edges g :b)

    (is (= [(e-> :a :loom [4] :b)] (loom/in-edges g :b)))

    (is (= 4 (loom/weight* g :a :b)))
    (is (= 9 (loom/weight* g :c :d)))
    (is (= 33 (loom/weight* g :a :d)))

    (is (= 4 (loom.attr/attrs g :a :b)))

    (is (= #{:a :b :c :d} (set (loom/nodes g))))

    (is (= [(e-> :a :loom [4] :b) (e-> :c :xyz :d)]
           (loom/edges g)))

    (is (= [(e-> :b :loom [4] :a) (e-> :d :xyz :c)]
           (loom/edges (loom/transpose g))))

    (is (loom/has-node? g :a))

    (is (loom/has-node? g :b))
    (is (loom/has-node? g :c))
    (is (loom/has-node? g :d))
    (is (not (loom/has-node? g :e)))
    (is (loom/has-edge? g :a :b))
    (is (not (loom/has-edge? g :a :c)))
    (is (not (loom/has-edge? g :d :b)))
    (is (not (loom/has-edge? g :a :x)))))

