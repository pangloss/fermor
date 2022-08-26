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

(deftest test-postwalk-state
  (is (= '["D" "B" "E" "C" "T" "A"]
        (postwalk-state (g/get-vertex simple-graph 'A) [] [:to]
          (fn [v state] (let [id (g/element-id v)]
                         (conj state (str id))))))))

(deftest test-reverse-postwalk
  (is (= '[A T C E B D]
        (reverse-postwalk (g/get-vertex simple-graph 'A) [:to] g/element-id))))

(deftest test-reverse-postwalk-state
  (is (= ["A" "T" "C" "E" "B" "D"]
        (reverse-postwalk-state (g/get-vertex simple-graph 'A) [] [:to]
          (fn [v state] (let [id (g/element-id v)]
                         (conj state (str id))))))))

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

(deftest test-postwalk-state-cyclic
  (is (= ["G" "D" "B" "M" "E" "C" "T" "X"]
        (postwalk-state (g/get-vertex cyclic-graph 'X) [] [:to]
          (fn [v state] (let [id (g/element-id v)]
                         (conj state (str id))))))))

(deftest test-reverse-postwalk-state-cyclic
  (is (= ["X" "T" "C" "E" "M" "B" "D" "G"]
        (reverse-postwalk-state (g/get-vertex cyclic-graph 'X) [] [:to]
          (fn [v state] (let [id (g/element-id v)]
                         (conj state (str id))))))))


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
