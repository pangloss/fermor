(ns fermor.custom-graph-test
  (:require [fermor.custom-graph :as custom]
            [fermor.core :refer :all :exclude [is]]
            [fermor.protocols :as p]
            [clojure.test :refer [deftest is testing]]))


;; Example

(custom/wrap-fn ->kind p/Kind)
(custom/extend-vertex p/Kind
                      [{:method :kind :wrap-fn ->kind}])

(deftype EExtended [edge]
  p/WeightedEdge
  (-weight [_] (:weight (get-document edge))))

(deftype VExtended [vertex]
  p/Kind
  (kind [_] (:kind (element-id vertex))))

(deftest different-weight-impl
  (let [orig-g (forked (->(graph) (add-edges :xy [[:a :b {:weight 12}] [:a :c]])))
        g (custom/wrap-graph orig-g nil nil ->VExtended ->EExtended)]
    (is (= #{12 nil}
           (set (->> (all-vertices g)
                     out-e
                     (map p/-weight)))))))

