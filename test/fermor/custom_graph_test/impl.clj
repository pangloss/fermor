(ns fermor.custom-graph-test.impl
  (:require [fermor.custom-graph :as custom]
            [fermor.core :refer :all :exclude [is]]
            [fermor.protocols :as p]))

;; Example 2: Add alter existing WeightedEdge protocol implementation and add Kind protocol.

(custom/extend-vertex p/Kind {})

(deftype EExtended [edge]
  p/WeightedEdge
  (-weight [_] (:weight (get-document edge))))

(deftype VExtended [vertex]
  p/Kind
  (kind [_] (:kind (element-id vertex))))


;; Example 2: Add arbitrary protocols to the graph with minimal boilerplate.

(defprotocol Region
  (member-edges [v])
  (members [v]))

(defprotocol HasRegion
  (region-edge [v])
  (region [v]))

(custom/extend-vertex Region {:member-edges [:edge] :members [:vertex]})
(custom/extend-vertex HasRegion {:region-edge :edge :region :vertex})

;; Look how I'm extending the Vertex type without reimplementing the entire set of classes!

(deftype VRegion [v]
  Region
  (member-edges [_] (in-e [:region] v))
  (members [_] (in [:region] v))
  HasRegion
  (region-edge [_] (first (out-edges v [:region])))
  (region [region-v] (some-> (region-edge region-v)
                             in-vertex)))

