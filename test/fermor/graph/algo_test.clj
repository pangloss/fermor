(ns fermor.graph.algo-test
  (:require [fermor.graph.algo :refer :all]
            [fermor.core :as g]
            [clojure.test :refer [deftest is testing]]))

;; NOTE REFERENCES have pictures of the graphs which can be quite helpful.
;; GRAPHS: "Notes on Graph Algorithms Used in Optimizing Compilers" by Carl D. Offner, 2013
;; ELI: https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/
;; DOM: "A Simple, Fast Dominance Algorithm" by Cooper, Harvey, and Kennedy, > 2001

(def simple-graph
  ;; from ELI
  (g/forked
    (g/add-edges 
      (g/graph)
      :to
      '[[A T] [A B] [A C] [T B] [C B] [B D] [C E] [E D]])))

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
  ;; from ELI
  (g/forked
    (g/add-edges (g/graph) :to
      '[[X T] [X B] [X C] [T B] [B D] [C E]
        [E D] [E M] [M C] [D G] [G D]])))

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
  ;; GRAPHS, p 24
  (g/forked
    (g/add-edges
      (g/graph)
      :to
      '[[S C] [S B] [S A] [C F] [C G] [F I] [G I]
        [G J] [I K] [K I] [K S] [B E] [B A] [E H]
        [H E] [H K] [A D]])))

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
        (immediate-dominators (g/get-vertex flow-graph 'S) [:to])))

  ;; TODO I'm really not sure if this is correct. It seems strange to me that
  ;; K's frontier is only I when it also points up to S and so apparently
  ;; dominates the whole graph? I don't have any cannonical dominance frontier
  ;; results to verify against.
  (is (= {(g/v 'A) #{}
          (g/v 'B) #{(g/v 'K) (g/v 'A)}
          (g/v 'C) #{(g/v 'I)}
          (g/v 'D) #{}
          (g/v 'E) #{(g/v 'K) (g/v 'E)}
          (g/v 'F) #{(g/v 'I)}
          (g/v 'G) #{(g/v 'I)}
          (g/v 'H) #{(g/v 'K) (g/v 'E)}
          (g/v 'I) #{(g/v 'K)}
          (g/v 'J) #{}
          (g/v 'K) #{(g/v 'I)}
          (g/v 'S) #{}}
        (dominance-frontiers (g/get-vertex flow-graph 'S) [:to])))

  (is (= (map set [[(g/v 'I) (g/v 'K) (g/v 'I)]
                   [(g/v 'I) (g/v 'K) (g/v 'S) (g/v 'C) (g/v 'F) (g/v 'I)]
                   [(g/v 'I) (g/v 'K) (g/v 'S) (g/v 'C) (g/v 'G) (g/v 'I)]
                   [(g/v 'B) (g/v 'E) (g/v 'H) (g/v 'K) (g/v 'S) (g/v 'B)]
                   [(g/v 'H) (g/v 'E) (g/v 'H)]])
        (map set (cycles flow-graph :to))))

  (is (= [#{(g/v 'S) (g/v 'A) (g/v 'F) (g/v 'D) (g/v 'B) (g/v 'J) (g/v 'C) (g/v 'G)}
          #{(g/v 'I)}
          #{(g/v 'K)}
          ;; NOTE: in the GRAPHS paper on page 35 it shows H and E as separate
          ;; intervals, so this may be wrong.
          #{(g/v 'H) (g/v 'E)}]
        (intervals (g/get-vertex flow-graph 'S) [:to]))))


(def irreducible-graph
  ;; Example from the DOM paper
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
        (immediate-dominators (g/get-vertex irreducible-graph 5) [:to])))

  (is (= [[(g/v 1) (g/v 2) (g/v 1)]]
        (cycles irreducible-graph :to))))


(def flow-graph2
  ;; page 41 of GRAPHS
  (g/forked (g/add-edges (g/graph) :to
              '[[S A] [S B] [A B] [B C] [C D] [C E] [D F]
                [E F] [E J] [E C] [F C] [F G] [F L] [I F]
                [G I] [G B] [J K] [K J] [L J] [L N] [L M]
                [M N] [M L] [N L] [M O] [G O] [H O] [H S]
                [G H] [O P] [P O] [P Q] [Q P]])))

(deftest flow-hard
  (is (= ;;
        {(g/v 'A) (g/v 'S) ;; ok
         (g/v 'B) (g/v 'S) ;; ok
         (g/v 'C) (g/v 'B) ;; ok
         (g/v 'D) (g/v 'C) ;; ok
         (g/v 'E) (g/v 'C) ;; ok
         (g/v 'F) (g/v 'C) ;; ok
         (g/v 'G) (g/v 'F) ;; ok
         (g/v 'H) (g/v 'G) ;; ok
         (g/v 'I) (g/v 'G) ;; ok
         (g/v 'J) (g/v 'C) ;; ok
         (g/v 'K) (g/v 'J) ;; ok
         (g/v 'L) (g/v 'F) ;; ok
         (g/v 'M) (g/v 'L) ;; ok
         (g/v 'N) (g/v 'L) ;; ok
         (g/v 'O) (g/v 'F) ;; ok
         (g/v 'P) (g/v 'O) ;; ok
         (g/v 'Q) (g/v 'P) ;; ok
         (g/v 'S) (g/v 'S)} ;; ok
        (immediate-dominators (g/get-vertex flow-graph2 'S) [:to]))
    "Dominators on p42 of GRAPHS")


  (is (= [#{(g/v 'S) (g/v 'A)}
          #{(g/v 'B)}
          #{(g/v 'D) (g/v 'C) (g/v 'E)}
          #{(g/v 'H) (g/v 'I) (g/v 'F) (g/v 'G)}
          #{(g/v 'N) (g/v 'L) (g/v 'M)}
          #{(g/v 'O)}
          #{(g/v 'Q) (g/v 'P)}
          #{(g/v 'K) (g/v 'J)}]
        (intervals (g/get-vertex flow-graph2 'S) [:to]))
    "Intervals on p45 of GRAPHS"))
