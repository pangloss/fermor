(ns fermor.force-atlas
  (:require [fermor.core :as g]
            [clojure.core.matrix :as cm :refer :all]
            [clojure.core.matrix.linear :as l]
            [clojure.core.reducers :as r]
            [fastmath.core :as fm :refer [atan2]]
            [fastmath.vector :as fmv]
            [untether.ugf :as ugf]))

;; The force atlas algo has 4 forces:
;; - friction
;; - gravity
;; - vertex repulsion (sometimes with anti-collision)
;; - edge pull
;;
;; The algo seems to work best with gravity turned off. Previously I'd get a nice state
;; with gravity off, then slowly ramp it back on to make the graph more dense again.
;; I think I can leave gravity off if I instead increase the edge pull.
;;
;; Increasing edge pull may have some nice possibilities. For instance I can identify
;; patterns in the graph and start the edge pull earlier or later on them, or make them
;; stronger or weaker.
;;
;; I would like to add a phase which induces rotation of formations such that the graph
;; becomes self-leveling. Perhaps edges could have some sort of buoyancy?
;;
;; Finally, I think the edge grouping is an optimization that is CPU specific and if I
;; move to matrix operations it may be more efficient to operate simply on nodes directly.
;; But even if not I think the grouping aspect is basically an add-on.
;;
;; There are a few oddball values that also get calculated like "traction",
;; which I use to wangle out and adjustment to the friction per-frame.

(def triples (map (fn [[from e to]]
                    [(ugf/id from) (rand-int 20) (ugf/id to)])
               (ugf/triples (ugf/read-ugf "/Users/dw/Downloads/bert-297.ugf"))))

