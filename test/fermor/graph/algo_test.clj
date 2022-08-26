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
        (immediate-dominators (g/get-vertex cyclic-graph 'X) [:to]))))



(deftest scc
  (is (= #{#{(g/v 'M) (g/v 'C) (g/v 'E)}
           #{(g/v 'D) (g/v 'G)}}
        (strongly-connected-components cyclic-graph :to false))))

(deftest on-the-range
  ;; TODO: I am not totally confident that this is correct.
  (is (= {(g/v 'X) #{},
          (g/v 'T) #{(g/v 'B)},
          (g/v 'C) #{(g/v 'D) (g/v 'C)},
          (g/v 'E) #{(g/v 'D) (g/v 'C)},
          (g/v 'M) #{(g/v 'C)},
          (g/v 'B) #{(g/v 'D)},
          (g/v 'D) #{(g/v 'D)},
          (g/v 'G) #{(g/v 'D)}}
        (dominance-frontiers (g/get-vertex cyclic-graph 'X) [:to]))))

(def flow-graph
  (g/forked
    (g/add-edges
      (g/graph)
      :to
      '[[S C]
        [S B]
        [S A]
        [C F]
        [C G]
        [F I]
        [G I]
        [G J]
        [I K]
        [K I]
        [K S]
        [B E]
        [B A]
        [E H]
        [H E]
        [H K]
        [A D]])))

(deftest flowy
  (is (= '[S C G J F B E H K I A D]
        (reverse-postwalk (g/get-vertex flow-graph 'S) :to g/element-id)))

  (is (= (mapv str (reverse-postwalk (g/get-vertex flow-graph 'S) :to g/element-id))
        (reverse-postwalk-reduce (g/get-vertex flow-graph 'S) [] [:to]
          (fn [state v] (conj state (str (g/element-id v)))))))

  (is (= {(g/v 'H) (g/v 'E),
          (g/v 'S) (g/v 'S),
          (g/v 'K) (g/v 'S),
          (g/v 'I) (g/v 'S),
          (g/v 'A) (g/v 'S),
          (g/v 'F) (g/v 'C),
          (g/v 'D) (g/v 'A),
          (g/v 'B) (g/v 'S),
          (g/v 'J) (g/v 'G),
          (g/v 'C) (g/v 'S),
          (g/v 'E) (g/v 'B),
          (g/v 'G) (g/v 'C)}
        (immediate-dominators (g/get-vertex flow-graph 'S) [:to]))))


(def irreducible-graph
  (g/forked
    (g/add-edges (g/graph) :to
      [[5 4]
       [5 3]
       [4 1]
       [1 2]
       [3 2]
       [2 1]])))

(deftest all-dom5
  (is (= {(g/v 5) (g/v 5)
          (g/v 4) (g/v 5)
          (g/v 3) (g/v 5)
          (g/v 2) (g/v 5)
          (g/v 1) (g/v 5)}
        (immediate-dominators (g/get-vertex irreducible-graph 5) [:to]))))
