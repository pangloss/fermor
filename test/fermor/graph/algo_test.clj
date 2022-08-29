(ns fermor.graph.algo-test
  (:require [fermor.graph.algo :refer :all]
            [fermor.core :as g :refer [v]]
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
        (postwalk-reduce (g/get-vertex simple-graph 'A) [:to] []
          (fn [state v]
            (conj state (str (g/element-id v))))))))

(deftest test-reverse-postwalk
  (is (= '[A T C E B D]
        (reverse-postwalk (g/get-vertex simple-graph 'A) [:to] g/element-id))))

(deftest test-reverse-postwalk-reduce
  (is (= ["A" "T" "C" "E" "B" "D"]
        (reverse-postwalk-reduce (g/get-vertex simple-graph 'A) [:to] []
          (fn [state v] (conj state (str (g/element-id v))))))))

(def cyclic-graph
  ;; from ELI
  (g/forked
    (g/add-edges (g/graph) :to
      '[[X T] [X B] [X C] [T B] [B D] [C E]
        [E D] [E M] [M C] [D G] [G D]])))


(deftest simple-graph-loops
  (is (= {}
        (find-loops (g/get-vertex simple-graph 'X) :to)))
  (is (= {[(v 'D) (v 'G)] {:loop-num 0, :parent nil, :depth 0},
          [(v 'C) (v 'M)] {:loop-num 1, :parent nil, :depth 0}}
        (find-loops (g/get-vertex cyclic-graph 'X) :to))))

(deftest test-postwalk-cyclic
  (is (= '[G D B M E C T X]
        (postwalk (g/get-vertex cyclic-graph 'X) :to g/element-id))))

(deftest test-reverse-postwalk-cyclic
  (is (= '[X T C E M B D G]
        (reverse-postwalk (g/get-vertex cyclic-graph 'X) :to g/element-id))))

(deftest test-postwalk-reduce-cyclic
  (is (= ["G" "D" "B" "M" "E" "C" "T" "X"]
        (postwalk-reduce (g/get-vertex cyclic-graph 'X) [:to] []
          (fn [state v] (conj state (str (g/element-id v))))))))

(deftest test-reverse-postwalk-reduce-cyclic
  (is (= ["X" "T" "C" "E" "M" "B" "D" "G"]
        (reverse-postwalk-reduce (g/get-vertex cyclic-graph 'X) [:to] []
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

(deftest scsg
  (let [g (strongly-connected-subgraphs cyclic-graph :to false (range))]
    (is (= #{(g/v 'M) (g/v 'C) (g/v 'E)}
          (g/vertices-with-edge g 0)))

    (is (= #{(g/v 'D) (g/v 'G)}
          (g/vertices-with-edge g 1)))

    (is (= 11
          (->> g g/vertices (g/out-e [:to]) count)))

    (is (= 5
          (->> g g/vertices (g/out-e [0 1 2]) count)))))

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
        (reverse-postwalk-reduce (g/get-vertex flow-graph 'S) [:to] []
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
        (intervals (g/get-vertex flow-graph 'S) [:to])))

  ;; This one is strange because I would expect K->I to be inside S->K, but
  ;; because the graph is irreducible and K->I is weirdly looping in the tail of
  ;; the S->K loop and it is entered from both S and K, it's ambiguous. The
  ;; traversal happened to produce this. I don't care much because I don't hope
  ;; to deal with such miserable graphs in reality.
  (is (= {[(v 'S) (v 'K)] {:loop-num 0, :parent nil, :depth 0},
          [(v 'E) (v 'H)] {:loop-num 1, :parent [(v 'S) (v 'K)], :depth 1},
          [(v 'K) (v 'I)] {:loop-num 2, :parent nil, :depth 0}}
        (find-loops (g/get-vertex flow-graph 'S) [:to]))))

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
        (cycles irreducible-graph :to)))

  (is (= {[(v 2) (v 1)] {:loop-num 0, :parent nil, :depth 0}}
        (find-loops (g/get-vertex irreducible-graph 5) [:to]))))

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
    "Intervals on p45 of GRAPHS")

  ;; this graph is really tricky. It's hard to be certain that this is correct, but
  ;; on visual inspection it looked reasonable.
  (is (= {[(v 'S) (v 'H)] {:loop-num 0 :parent nil :depth 0}
          [(v 'B) (v 'G)] {:loop-num 1 :parent [(v 'S) (v 'H)] :depth 1}
          [(v 'C) (v 'F)] {:loop-num 2 :parent [(v 'B) (v 'G)] :depth 2}
          [(v 'C) (v 'E)] {:loop-num 3 :parent [(v 'C) (v 'F)] :depth 3}
          [(v 'F) (v 'I)] {:loop-num 4 :parent [(v 'C) (v 'E)] :depth 3}
          [(v 'L) (v 'N)] {:loop-num 5 :parent [(v 'F) (v 'I)] :depth 4}
          [(v 'L) (v 'M)] {:loop-num 6 :parent [(v 'L) (v 'N)] :depth 5}
          [(v 'J) (v 'K)] {:loop-num 7 :parent [(v 'L) (v 'M)] :depth 6}
          [(v 'O) (v 'P)] {:loop-num 8 :parent [(v 'F) (v 'I)] :depth 4}
          [(v 'P) (v 'Q)] {:loop-num 9 :parent [(v 'F) (v 'I)] :depth 4}}
        (find-loops (g/get-vertex flow-graph2 'S) [:to]))))

(def loops-graph
  (g/forked (g/add-edges (g/graph) :to
              [[1 2] [1 3] [2 3] [3 4]
               [4 5] [4 6] [5 7] [6 7]
               [4 3] [7 4] [7 8] [8 3]
               [8 9] [9 1] [8 10] [10 7]])))

(deftest loops-test
  (is (= {(g/v 7) (g/v 4),
          (g/v 1) (g/v 1),
          (g/v 4) (g/v 3),
          (g/v 6) (g/v 4),
          (g/v 3) (g/v 1),
          (g/v 2) (g/v 1),
          (g/v 9) (g/v 8),
          (g/v 5) (g/v 4),
          (g/v 10) (g/v 8),
          (g/v 8) (g/v 7)}
        (immediate-dominators (g/get-vertex loops-graph 1) :to)))

  (is (= (range 10)
        (->> (reverse-post-order-numbering (g/get-vertex loops-graph 1) :to)
          vals
          sort))
    "this had a bug where a node appeared twice in the traversal, throwing the numbering out.")

  (is (= {[(v 1) (v 9)] {:loop-num 0, :parent nil, :depth 0, :nesting []},
          [(v 3) (v 8)]
          {:loop-num 1, :parent [(v 1) (v 9)], :depth 1, :nesting [[(v 1) (v 9)]]},
          [(v 3) (v 4)]
          {:loop-num 2,
           :parent [(v 3) (v 8)],
           :depth 2,
           :nesting [[(v 1) (v 9)] [(v 3) (v 8)]]},
          [(v 4) (v 7)]
          {:loop-num 3,
           :parent [(v 3) (v 8)],
           :depth 2,
           :nesting [[(v 1) (v 9)] [(v 3) (v 8)]]},
          [(v 7) (v 10)]
          {:loop-num 4,
           :parent [(v 3) (v 8)],
           :depth 2,
           :nesting [[(v 1) (v 9)] [(v 3) (v 8)]]}}
        (find-loops (g/get-vertex loops-graph 1) :to))))
