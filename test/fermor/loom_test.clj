(ns fermor.loom-test
  (:require [clojure.test :refer [deftest testing is are]]
            [fermor.core :refer [graph linear forked add-edges add-vertices v e-> e->in
                                 undirected-edge]]
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

    (is (= [[:a :b]] (loom/out-edges g :a)))

    (is (= [:a] (loom/predecessors* g :b)))
    (is (= 1 (loom/in-degree g :b)))

    (loom/in-edges g :b)

    (is (= [[:a :b]] (loom/in-edges g :b)))

    (is (= 4 (loom/weight* g :a :b)))
    (is (= 9 (loom/weight* g :c :d)))
    (is (= 33 (loom/weight* g :a :d)))

    (is (= 4 (loom.attr/attrs g :a :b)))

    (is (= #{:a :b :c :d} (set (loom/nodes g))))

    (is (= [[:a :b] [:c :d]]
           (loom/edges g)))

    (is (= [[:b :a] [:d :c]]
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


;; Loom has these compliance tests but they aren't part of the release, so copied them here:

(defn graph-test
  "Collection of simple graph tests. Uses the provided empty graph instance, g, to create
  various graphs and test the implementation."
  [g]
  (let [g1 (-> g linear (loom/add-edges [1 2] [1 3] [2 3]) (loom/add-nodes 4) forked)

        g4 (-> g1 linear (loom/add-edges [5 6] [7 8]) (loom/add-nodes 9) forked)
        g5 g]
    (testing "Construction, loom/nodes, loom/edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (loom/nodes g1))
           #{[1 2] [2 1] [1 3] [3 1] [2 3] [3 2]} (set (loom/edges g1))
           #{1 2 3 4 5 6 7 8 9} (set (loom/nodes g4))
           #{[1 2] [2 1] [1 3] [3 1] [2 3]
             [3 2] [5 6] [6 5] [7 8] [8 7]} (set (loom/edges g4))
             #{} (set (loom/nodes g5))
             #{} (set (loom/edges g5))
             ;true (loom/has-node? g1 4)
             true (loom/has-edge? g1 1 2)
             false (loom/has-node? g1 5)
             false (loom/has-edge? g1 4 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (loom/successors g1 1))
           #{1 2} (set (loom/successors g1 3))
           #{} (set (loom/successors g1 4))
           2 (loom/out-degree g1 1)
           2 (loom/out-degree g1 3)
           0 (loom/out-degree g1 4)))
    #_
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (loom/nodes (loom/add-nodes g1 5)))
           #{:a :b :c} (set (loom/nodes (loom/add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (loom/nodes (loom/add-nodes g5 {:id 1} {:id 2})))
           #{[1 2] [2 1]} (set (loom/edges (loom/add-edges g5 [1 2])))
           #{1 2} (set (loom/nodes (loom/remove-nodes g1 3 4)))
           #{[1 2] [2 1]} (set (loom/edges (loom/remove-nodes g1 3 4)))
           #{1 2 3 4} (set (loom/nodes (loom/remove-edges g1 [1 2] [2 1] [1 3] [3 1])))
           #{[2 3] [3 2]} (set (loom/edges (loom/remove-edges
                                            g1 [1 2] [2 1] [1 3] [3 1])))))
    #_
    (testing "Adding multiple loom/edges"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (loom/nodes (loom/add-edges* g5 [[1 2] [2 3] [3 4] [4 5]])))
           #{[1 2] [2 1] [2 3] [3 2] [3 4] [4 3] [4 5] [5 4]} (set (loom/edges (loom/add-edges* g5 [[1 2] [2 3] [3 4] [4 5]])))))))

(defn digraph-test
  "Test the provided digraph implementation. The dg parameter is a digraph instance and may be used to construct
  other digraphs for testing."
  [dg]
  (let [g1 (-> dg linear (loom/add-edges [1 2] [1 3] [2 3]) (loom/add-nodes 4) forked)
        g4 (-> g1 linear (loom/add-edges [5 6] [6 5] [7 8]) (loom/add-nodes 9) forked)
        g5 dg
        g6 (loom/transpose g1)]
    (testing "Construction, loom/nodes, loom/edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (loom/nodes g1))
           #{1 2 3 4} (set (loom/nodes g6))
           #{[1 2] [1 3] [2 3]} (set (loom/edges g1))
           #{[2 1] [3 1] [3 2]} (set (loom/edges g6))
           #{1 2 3 4 5 6 7 8 9} (set (loom/nodes g4))
           #{[1 2] [1 3] [2 3] [5 6] [6 5] [7 8]} (set (loom/edges g4))
           true (loom/has-node? g1 4)
           true (loom/has-edge? g1 1 2)
           false (loom/has-node? g1 5)
           false (loom/has-edge? g1 2 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (loom/successors g1 1))
           #{} (set (loom/successors g1 3))
           #{} (set (loom/successors g1 4))
           2 (loom/out-degree g1 1)
           0 (loom/out-degree g1 3)
           0 (loom/out-degree g1 4)
           #{1 2} (set (loom/predecessors g1 3))
           #{} (set (loom/predecessors g1 1))
           2 (loom/in-degree g1 3)
           0 (loom/in-degree g1 1)
           #{1 2} (set (loom/successors g6 3))
           #{} (set (loom/successors g6 1))
           2 (loom/out-degree g6 3)
           0 (loom/out-degree g6 1)))
    #_
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (loom/nodes (loom/add-nodes g1 5)))
           #{:a :b :c} (set (loom/nodes (loom/add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (loom/nodes (loom/add-nodes g5 {:id 1} {:id 2})))
           #{[1 2]} (set (loom/edges (loom/add-edges g5 [1 2])))
           #{1 2} (set (loom/nodes (loom/remove-nodes g1 3 4)))
           #{[1 2]} (set (loom/edges (loom/remove-nodes g1 3 4)))
           #{1 2 3 4} (set (loom/nodes (loom/remove-edges g1 [1 2] [1 3])))
           #{[2 3]} (set (loom/edges (loom/remove-edges g1 [1 2] [1 3])))))))

(defn weighted-graph-test
  [wg]
  (let [g1 (-> wg linear (loom/add-edges [1 2 77] [1 3 88] [2 3 99]) (loom/add-nodes 4) forked)
        g4 (-> g1 linear (loom/add-edges [5 6 88] [7 8]) (loom/add-nodes 9) forked)
        g5 wg]
    (testing "Construction, loom/nodes, loom/edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (loom/nodes g1))
           #{[1 2] [2 1] [1 3] [3 1] [2 3] [3 2]} (set (loom/edges g1))
           #{1 2 3 4 5 6 7 8 9} (set (loom/nodes g4))
           #{[1 2] [2 1] [1 3] [3 1] [2 3]
             [3 2] [5 6] [6 5] [7 8] [8 7]} (set (loom/edges g4))
             #{} (set (loom/nodes g5))
             #{} (set (loom/edges g5))
             true (loom/has-node? g1 4)
             true (loom/has-edge? g1 1 2)
             false (loom/has-node? g1 5)
             false (loom/has-edge? g1 4 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (loom/successors g1 1))
           #{1 2} (set (loom/successors g1 3))
           #{} (set (loom/successors g1 4))
           2 (loom/out-degree g1 1)
           2 (loom/out-degree g1 3)
           0 (loom/out-degree g1 4)))
    #_
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (loom/nodes (loom/add-nodes g1 5)))
           #{:a :b :c} (set (loom/nodes (loom/add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (loom/nodes (loom/add-nodes g5 {:id 1} {:id 2})))
           #{[1 2] [2 1]} (set (loom/edges (loom/add-edges g5 [1 2])))
           #{1 2} (set (loom/nodes (loom/remove-nodes g1 3 4)))
           #{[1 2] [2 1]} (set (loom/edges (loom/remove-nodes g1 3 4)))
           #{1 2 3 4} (set (loom/nodes (loom/remove-edges g1 [1 2] [2 1] [1 3] [3 1])))
           #{[2 3] [3 2]} (set (loom/edges (loom/remove-edges g1 [1 2] [2 1] [1 3] [3 1])))))
    (testing "Weight"
      (are [expected got] (== expected got)
           77 (loom/weight g1 1 2)
           88 (loom/weight g4 6 5)
           1 (loom/weight g4 7 8)))))

(defn weighted-digraph-test
  [dwg]
  (let [g1 (-> dwg linear (loom/add-edges [1 2 77] [1 3 88] [2 3 99]) (loom/add-nodes 4) forked)
        g4 (-> g1 linear (loom/add-edges [5 6 88] [6 5 88] [7 8]) (loom/add-nodes 9) forked)
        g5 dwg
        g6 (loom/transpose g1)]
    (testing "Construction, loom/nodes, loom/edges"
      (are [expected got] (= expected got)
           #{1 2 3 4} (set (loom/nodes g1))
           #{1 2 3 4} (set (loom/nodes g6))
           #{[1 2] [1 3] [2 3]} (set (loom/edges g1))
           #{[2 1] [3 1] [3 2]} (set (loom/edges g6))
           #{1 2 3 4 5 6 7 8 9} (set (loom/nodes g4))
           #{[1 2] [1 3] [2 3] [5 6] [6 5] [7 8]} (set (loom/edges g4))
           #{} (set (loom/nodes g5))
           #{} (set (loom/edges g5))
           true (loom/has-node? g1 4)
           true (loom/has-edge? g1 1 2)
           false (loom/has-node? g1 5)
           false (loom/has-edge? g1 2 1)))
    (testing "Successors"
      (are [expected got] (= expected got)
           #{2 3} (set (loom/successors g1 1))
           #{} (set (loom/successors g1 3))
           #{} (set (loom/successors g1 4))
           2 (loom/out-degree g1 1)
           0 (loom/out-degree g1 3)
           0 (loom/out-degree g1 4)
           #{1 2} (set (loom/predecessors g1 3))
           #{} (set (loom/predecessors g1 1))
           2 (loom/in-degree g1 3)
           0 (loom/in-degree g1 1)
           #{1 2} (set (loom/successors g6 3))
           #{} (set (loom/successors g6 1))
           2 (loom/out-degree g6 3)
           0 (loom/out-degree g6 1)))
    #_
    (testing "Add & remove"
      (are [expected got] (= expected got)
           #{1 2 3 4 5} (set (loom/nodes (loom/add-nodes g1 5)))
           #{:a :b :c} (set (loom/nodes (loom/add-nodes g5 :a :b :c)))
           #{{:id 1} {:id 2}} (set (loom/nodes (loom/add-nodes g5 {:id 1} {:id 2})))
           #{[1 2]} (set (loom/edges (loom/add-edges g5 [1 2])))
           #{1 2} (set (loom/nodes (loom/remove-nodes g1 3 4)))
           #{[1 2]} (set (loom/edges (loom/remove-nodes g1 3 4)))
           #{1 2 3 4} (set (loom/nodes (loom/remove-edges g1 [1 2] [1 3])))
           #{[2 3]} (set (loom/edges (loom/remove-edges g1 [1 2] [1 3])))))
    (testing "Weight"
      (are [expected got] (== expected got)
           77 (loom/weight g1 1 2)
           77 (loom/weight g6 2 1)
           88 (loom/weight g4 6 5)
           1 (loom/weight g4 7 8)))))

(deftest do-graph-test
  (graph-test (forked (as-loom-graph (add-edges (graph) :loom undirected-edge [])))))

(deftest do-digraph-test
  (digraph-test (forked (as-loom-graph (graph)))))

(deftest do-weighted-graph-test
  (weighted-graph-test (forked (as-loom-graph (add-edges (graph) :loom undirected-edge [])))))

(deftest do-weighted-digraph-test
  (weighted-digraph-test (forked (as-loom-graph (graph)))))
