(ns fermor.graph.lattice-test
  (:require [clojure.test :refer [deftest testing is]]
            [fermor.protocols :refer [get-vertex element-id vertex?]]
            [fermor.core :as g]
            [fermor.graph.lattice :refer :all]))

(deftest simple-example
  (let [g (g/forked
            (g/add-edges (g/graph) :x
              '[[top a]
                [top b]
                [a a']
                [a' bottom]
                [b bottom]]))
        l (symmetric-lattice g :x)
        v #(g/get-vertex g %)]

    (is (symmetric? l))

    (is (= (v 'bottom)
          (meet l (v 'a) (v 'b))))

    (is (= (v 'top)
          (join l (v 'a) (v 'b))))

    (is (= (v 'a')
          (meet l (v 'a) (v 'a'))))

    (is (= (v 'a)
          (join l (v 'a) (v 'a'))))

    (let [g (g/forked
              (g/add-edges (g/linear g) :x
                '[[a bad]
                  [bad b]]))]

      (is (thrown? Exception (symmetric-lattice g :x)))
      ;; If the lattice were created, it would have trickier behavior:
      ;; meet and join here lose their symmetry.
      #_(is (= (v 'b)   (meet l (v 'a) (v 'b))))
      #_(is (= (v 'top) (join l (v 'a) (v 'b))))

      #_(is (= (v 'a')  (meet l (v 'a) (v 'a'))))
      #_(is (= (v 'a)   (join l (v 'a) (v 'a')))))))


(def types-graph
  (g/forked
    (g/add-edges (g/graph) :lower-type
      '[[top any-risc-scalar]
        [top U]
        [top any-pe-byte]

        [any-risc-scalar any-i32]
        [any-risc-scalar const-true]
        [any-risc-scalar const-false]
        [any-risc-scalar any-struct-ptr]

        [any-i32 const-i32-a]
        [any-i32 const-i32-b]
        [any-i32 any-ui32]
        [any-ui32 const-ui32-a]
        [any-ui32 const-ui32-b]
        [any-struct-ptr const-struct-ptr-a]
        [any-struct-ptr const-struct-ptr-b]
        [any-struct-ptr const-nil]

        [U R]
        [any-pe-byte unknown-pe-byte]

        [const-nil unknown-struct-ptr]
        [const-struct-ptr-b unknown-struct-ptr]
        [const-struct-ptr-a unknown-struct-ptr]
        [const-ui32-b unknown-ui32]
        [const-ui32-a unknown-ui32]
        [unknown-ui32 unknown-i32]
        [const-i32-b unknown-i32]
        [const-i32-a unknown-i32]

        [unknown-struct-ptr unknown-risc-scalar]
        [const-false unknown-risc-scalar]
        [const-true unknown-risc-scalar]
        [unknown-i32 unknown-risc-scalar]

        [unknown-pe-byte bottom]
        [R bottom]
        [unknown-risc-scalar bottom]])))

(def types (symmetric-lattice types-graph :lower-type))

(defn v [s]
  (g/get-vertex types-graph s))

(deftest meetings
  (is (= (v 'bottom)
        (meet types (v 'R) (v 'const-false))))

  (is (= (v 'R)
        (meet types (v 'R) (v 'U))))

  (is (= (v 'unknown-struct-ptr)
        (meet types (v 'const-struct-ptr-a) (v 'const-struct-ptr-b))))

  (is (= (v 'unknown-risc-scalar)
        (meet types (v 'const-true) (v 'unknown-struct-ptr)))))

(deftest joinings
  (is (= (v 'top)
        (join types (v 'R) (v 'const-false))))

  (is (= (v 'U)
        (join types (v 'R) (v 'U))))

  (is (= (v 'any-struct-ptr)
        (join types (v 'const-struct-ptr-a) (v 'const-struct-ptr-b))))

  (is (= (v 'any-risc-scalar)
        (join types (v 'const-true) (v 'unknown-struct-ptr)))))

(deftest symmetry
  (is (symmetric? types)))
