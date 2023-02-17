(ns fermor.ui
  (:require [membrane.ui :as ui]
            [membrane.skia :as backend]
            [clojure.java.io :as io]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defeffect defui make-app]]
            [fermor.force-atlas :as fa]
            [fermor.force-atlas.graph :as fg]
            [fermor.core :as g])
  (:import [fastmath.vector Vec2]))


(def node-size 3)
(def node-radius (/ node-size 2))

(defn arrow
  ([x y]
   (ui/with-style :membrane.ui/style-stroke
     (ui/path [0 0]
              [x y]))))

(defn g->view [g]
  (let [poss (->> g g/vertices g/documents (map fermor.force-atlas.graph/position))

        minx (->> poss
                  (map first)
                  (apply min))
        maxx (->> poss
                  (map first)
                  (apply max))

        miny (->> poss
                  (map second)
                  (apply min))
        maxy (->> poss
                  (map second)
                  (apply max))



        verts-view
        (into []
              (map (fn [[x y :as pos]]
                     (let []
                       (ui/translate (- x node-radius) (- y node-radius)
                                     (ui/with-style :membrane.ui/style-stroke
                                       (ui/rounded-rectangle node-size node-size
                                                             node-radius))))))
              poss)

        edges-view
        (into []
              (comp
               
               (map
                (fn [vert]
                  (let [[vx vy] (-> vert
                                    g/get-document
                                    fg/position)]
                    (ui/translate vx vy
                                  (into []
                                        (map (fn [out-vert]
                                               (let [[x y] (-> out-vert
                                                               g/get-document
                                                               fg/position)]
                                                 (arrow (- x vx) (- y vy)))))
                                        
                                        (g/out vert)))))))
              (g/vertices g))

        gv
        (ui/padding 100 100
                    (ui/translate (- minx) (- miny)
                                  [verts-view
                                   edges-view]))]
    gv))



(defn aspect-fit [view w h]
  (let [[vw vh] (ui/bounds view)
        wratio (/ w (max 1 vw))
        hratio (/ h (max 1 vh))

        ratio (min wratio hratio)]
    (ui/scale ratio ratio
              view)))

(defui layout-viewer [{:keys [n layouts]}]
  (ui/translate
   50 50
   (ui/vertical-layout
    (basic/number-slider {:num n
                          :min 0
                          :max-width 400
                          :integer? true
                          :max (dec (count layouts))})
    (let [layout (nth layouts n)]
      (aspect-fit layout 200 200)))))

(defn -main [& args]
  (let [;; this can take a while
        layouts (into []
                      (comp
                       (take 800)
                       (map g->view))
                      (iterate fa/force-atlas
                               (let [n 500]
                                 (fg/make-graph (for [^long i (range n)]
                                                  (let [a (mod i 20)
                                                        b (if (= a i) (inc i) i)]
                                                    [a (rand) b]))))))]
    (backend/run-sync
      (make-app #'layout-viewer {:n 0
                                 :layouts layouts}))))

(comment
  (let [;; this can take a while
        layouts (into []
                      (comp
                       (take 800)
                       (map g->view))
                      (iterate fa/force-atlas
                               (let [n 500]
                                 (fg/make-graph (for [^long i (range n)]
                                                  (let [a (mod i 20)
                                                        b (if (= a i) (inc i) i)]
                                                    [a (rand) b]))))))]
    (backend/run
     (make-app #'layout-viewer {:n 0
                                :layouts layouts})))
  ,)







