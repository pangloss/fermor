(ns fermor.graph.algo-test
  (:require [fermor.graph.algo :refer :all]
            [fermor.core :as g]
            [clojure.test :refer [deftest is testing]]))

(def simple-graph
  (g/forked
    (g/add-edges 
      (g/graph)
      :to
      '[[A T]
        [A B]
        [A C]
        [T B]
        [C B]
        [B D]
        [C E]
        [E D]])))

(deftest test-postwalk
  (is (= '[D B E C T A]
        (postwalk (g/get-vertex simple-graph 'A) [:to] g/element-id))))

(deftest test-reverse-postwalk
  (is (= '[A T C E B D]
        (reverse-postwalk (g/get-vertex simple-graph 'A) [:to] g/element-id))))

(deftest test-postwalk-reduce
  (is (= '["D" "B" "E" "C" "T" "A"]
        (postwalk-reduce (g/get-vertex simple-graph 'A) [] [:to]
          (fn [state v]
            (conj state (str (g/element-id v))))))))

(deftest test-reverse-postwalk
  (is (= '[A T C E B D]
        (reverse-postwalk (g/get-vertex simple-graph 'A) [:to] g/element-id))))

(deftest test-reverse-postwalk-reduce
  (is (= ["A" "T" "C" "E" "B" "D"]
        (reverse-postwalk-reduce (g/get-vertex simple-graph 'A) [] [:to]
          (fn [state v] (conj state (str (g/element-id v))))))))

(def cyclic-graph
  (g/forked
    (g/add-edges
      (g/graph)
      :to
      '[[X T]
        [X B]
        [X C]
        [T B]
        [B D]
        [C E]
        [E D]
        [E M]
        [M C]
        [D G]
        [G D]])))

(deftest test-postwalk-cyclic
  (is (= '[G D B M E C T X]
        (postwalk (g/get-vertex cyclic-graph 'X) :to g/element-id))))

(deftest test-reverse-postwalk-cyclic
  (is (= '[X T C E M B D G]
        (reverse-postwalk (g/get-vertex cyclic-graph 'X) :to g/element-id))))

(deftest test-postwalk-reduce-cyclic
  (is (= ["G" "D" "B" "M" "E" "C" "T" "X"]
        (postwalk-reduce (g/get-vertex cyclic-graph 'X) [] [:to]
          (fn [state v] (conj state (str (g/element-id v))))))))

(deftest test-reverse-postwalk-reduce-cyclic
  (is (= ["X" "T" "C" "E" "M" "B" "D" "G"]
        (reverse-postwalk-reduce (g/get-vertex cyclic-graph 'X) [] [:to]
          (fn [state v] (conj state (str (g/element-id v))))))))


(deftest dominance
  (is (= {(g/v 'X) (g/v 'X)
          (g/v 'T) (g/v 'X)
          (g/v 'C) (g/v 'X)
          (g/v 'E) (g/v 'C)
          (g/v 'M) (g/v 'E)
          (g/v 'B) (g/v 'X)
          (g/v 'D) (g/v 'X)
          (g/v 'G) (g/v 'D)}
        (dominators (g/get-vertex cyclic-graph 'X) [:to]))))



(deftest scc
  (is (= #{#{(g/v 'M) (g/v 'C) (g/v 'E)}
           #{(g/v 'D) (g/v 'G)}}
        (strongly-connected-components cyclic-graph :to false))))
