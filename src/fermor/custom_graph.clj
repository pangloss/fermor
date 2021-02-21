(ns fermor.custom-graph
  (:use fermor.protocols)
  (:require [clojure.pprint :refer [simple-dispatch]]
            fermor.graph))

(defprotocol IWrapper
  (-wrapper-settings [w])
  (set-F! [w f])
  (set-L! [w l])
  (set-E! [w e])
  (set-V! [w v])
  (forked-graph-wrapper [w])
  (linear-graph-wrapper [w])
  (edge-wrapper [w])
  (vertex-wrapper [w])
  (implementor-type ^Class [w])
  (make-implementor [w]))

(deftype Wrapper [settings t -> ^:unsynchronized-mutable F ^:unsynchronized-mutable L ^:unsynchronized-mutable V ^:unsynchronized-mutable E]
  IWrapper
  (-wrapper-settings [w] settings)
  (set-F! [w f] (set! F f))
  (set-L! [w l] (set! L l))
  (set-E! [w e] (set! E e))
  (set-V! [w v] (set! V v))
  (forked-graph-wrapper [w] F)
  (linear-graph-wrapper [w] L)
  (edge-wrapper [w] E)
  (vertex-wrapper [w] V)
  (implementor-type ^Class [w] t)
  (make-implementor [w] ->))

(declare ->mE ->mV ->mForkedGraph ->mLinearGraph)


(defn wrap-graph [graph settings ->FImpl ->LImpl ->VImpl ->EImpl]
  ;; default type to Object which won't implement the desired interfaces so will
  ;; never trigger use of the make-implementor, without requiring any other special
  ;; conditional logic.
  (let [ForkedGraphImplType (if ->FImpl (type (->FImpl nil)) Object)
        LinearGraphImplType (if ->LImpl (type (->LImpl nil)) Object)
        VertexImplType      (if ->VImpl (type (->VImpl nil)) Object)
        EdgeImplType        (if ->EImpl (type (->EImpl nil)) Object)
        no-wrapper (fn [x _] x)
        F+ (Wrapper. settings ForkedGraphImplType (or ->FImpl no-wrapper) nil nil nil nil)
        L+ (Wrapper. settings LinearGraphImplType (or ->LImpl no-wrapper) nil nil nil nil)
        E+ (Wrapper. settings EdgeImplType (or ->EImpl no-wrapper) nil nil nil nil)
        V+ (Wrapper. settings VertexImplType (or ->VImpl no-wrapper) nil nil nil nil)]
    (doseq [[setter value] [[set-E! E+] [set-V! V+] [set-F! F+] [set-L! L+]]
            target [E+ V+ F+ L+]]
       (setter target value))
    (condp satisfies? graph
      Forked (->mForkedGraph graph F+)
      Linear (->mLinearGraph graph L+)
      Vertex (->mV graph V+)
      Edge   (->mE graph E+))))

(defmacro wrap-fn* [interface]
  `(let [interface# (if (map? ~interface)
                      (:on-interface ~interface) ;; get interface from protocol
                      ~interface)]
     (fn [wrapper# element#]
       (if (.isAssignableFrom interface# (implementor-type wrapper#))
         ((make-implementor wrapper#) element#)
         (plain element#)))))

(defmacro wrap-fn [name interface]
  `(let [interface# (if (map? ~interface)
                     (:on-interface ~interface) ;; get interface from protocol
                     ~interface)]
     (defn ~name [wrapper# element#]
       (if (.isAssignableFrom interface# (implementor-type wrapper#))
         ((make-implementor wrapper#) element#)
         (plain element#)))))

(defmacro wrap-inline [name interface]
  `(defmacro ~name [wrapper# element#]
     `(if (.isAssignableFrom ~~interface (implementor-type ~wrapper#))
        ((make-implementor ~wrapper#) ~element#)
        (plain ~element#))))

(do
  ;; NOTE: if any of these give problems, switch to wrap-fn to debug.
  (wrap-inline ->vertex-edges fermor.protocols.VertexEdges)
  (wrap-inline ->vertex-edges-prepared fermor.protocols.VertexEdgesPrepared)
  (wrap-inline ->element-id fermor.protocols.Element)
  (wrap-inline ->get-graph fermor.protocols.Element)
  (wrap-inline ->get-document fermor.protocols.Element)
  (wrap-inline ->weight fermor.protocols.WeightedEdge)
  (wrap-inline ->label fermor.protocols.Edge)
  (wrap-inline ->edge fermor.protocols.Edge)
  (wrap-inline ->settings fermor.protocols.Graph)
  (wrap-inline ->all-vertices fermor.protocols.Graph)
  (wrap-inline ->get-vertex fermor.protocols.Graph)
  (wrap-inline ->to-linear fermor.protocols.Forked)
  (wrap-inline ->to-forked fermor.protocols.Linear)
  (wrap-inline ->graph-contents fermor.protocols.GraphContents)
  (wrap-inline ->get-edge fermor.protocols.GetEdge)
  (wrap-inline ->transpose fermor.protocols.GraphTranspose))

(comment (macroexpand '(->vertex-edges 'element 'V+)))


(defprotocol Wrapping
  (plain [w])
  (-wrapper [w]))

(deftype mV [element V+]
  Object
  (equals [a b] (= element b))
  (hashCode [e] (.hashCode element))

  Element
  (element-id [v] (-> V+ (->element-id v) element-id))
  (get-graph [v] (-> V+ (->get-graph v) get-graph))
  (get-document [v] (-> V+ (->get-document v) get-document))
  (get-document [v key] (-> V+ (->get-document v) (get-document key)))

  Wrappable
  (-unwrap [e] (-unwrap element))

  Wrapping
  (plain [w] element)
  (-wrapper [w] V+)

  VertexEdges
  (-out-edges [v]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex-edges v) -out-edges))))
  (-out-edges [v labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex-edges v) (-out-edges labels)))))
  (-in-edges [v]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex-edges v) -in-edges))))
  (-in-edges [v labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex-edges v) (-in-edges labels)))))

  VertexEdgesPrepared
  (-out-edges [v prepared-labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> (->vertex-edges-prepared V+ v)
               (-out-edges-prepared prepared-labels)))))
  (-in-edges [v prepared-labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> (->vertex-edges-prepared V+ v)
               (-in-edges-prepared prepared-labels))))))

(deftype mE [element E+]
  Object
  (equals [a b] (.equals element b))
  (hashCode [e] (.hashCode element))

  Element
  (element-id [e] (-> E+ (->element-id e) element-id))
  (get-graph [e] (-> E+ (->get-graph e) get-graph))
  (get-document [e] (-> E+ (->get-document e) get-document))

  Wrappable
  (-unwrap [e] (-unwrap element))

  Wrapping
  (plain [w] element)
  (-wrapper [w] E+)

  WeightedEdge
  (-weight [e] (-> E+ (->weight e) -weight))

  Edge
  (-label [e] (-> E+ (->label e) -label))
  (in-vertex [e] (-> E+ (->edge e) in-vertex (->mV (vertex-wrapper E+))))
  (out-vertex [e] (-> E+ (->edge e) out-vertex (->mV (vertex-wrapper E+)))))

(deftype mForkedGraph [graph F+]
  Object
  (equals [a b] (.equals graph b))
  (hashCode [e] (.hashCode graph))

  Graph
  (-settings [g]
    (-> F+ (->settings g) -settings))
  (all-vertices [g]
    (let [V+ (vertex-wrapper F+)]
      (map #(->mV % V+) (-> F+ (->all-vertices g) all-vertices))))
  (get-vertex [g id]
    (some-> (get-vertex (->get-vertex F+ g) id) (->mV (vertex-wrapper F+))))

  GraphContents
  (-has-vertex-document? [g id]
    (-> F+ (->graph-contents g) (-has-vertex-document? id)))
  (-has-vertex? [g id labels]
    (-> F+ (->graph-contents g) (-has-vertex? id labels)))
  (-has-vertex? [g id]
    (-> F+ (->graph-contents g) (-has-vertex? id)))

  GetEdge
  (-get-edge [g label from-id to-id]
    (some-> (-get-edge (->get-edge F+ g) label from-id to-id)
            (->mE (edge-wrapper F+))))

  Wrapping
  (plain [w] graph)
  (-wrapper [w] F+)

  Wrappable
  (-unwrap [g]
    (-unwrap graph))

  Forked
  (to-linear [g]
    (-> F+ (->to-linear g) to-linear (->mLinearGraph (linear-graph-wrapper F+)))))

;; I'm not sure if this is needed and need to look closely at the behavior of
;; these internal components to figure out the wrapping story. so for now the
;; LinearGraph wrapper won't work even though it's defined above.
(deftype mLinearGraph [graph L+]
  Object
  (equals [a b] (.equals graph b))
  (hashCode [e] (.hashCode graph))

  ;; clojure.lang.IObj
  ;; (withMeta [o m]
  ;;   (if (= m metadata)
  ;;     o
  ;;     (->mLinearGraph edges documents settings m)))

  ;; clojure.lang.IMeta
  ;; (meta [o] (-> L+ (->meta graph) metadata))

  Wrapping
  (plain [w] graph)
  (-wrapper [w] L+)

  GraphTranspose
  (-transpose [g]
    (-> L+ (->transpose g) -transpose (->mLinearGraph L+)))
  (-transpose [g labels]
    (-> L+ (->transpose g) (-transpose labels) (->mLinearGraph L+)))

  Linear
  (to-forked [g]
    (-> L+ (->to-forked g) to-forked (->mForkedGraph (forked-graph-wrapper L+)))))

(defmethod print-method mE [^mE e ^java.io.Writer w]
  (print-method (plain e) w))

(defmethod simple-dispatch mE [o]
  (print-method o *out*))

(defmethod print-method mV [e ^java.io.Writer w]
  (print-method (plain e) w))

(defmethod simple-dispatch mV [o]
  (print-method o *out*))

(defmethod print-method mE [^mE e ^java.io.Writer w]
  (print-method (.element e) w))

(defmethod simple-dispatch mE [o]
  (print-method o *out*))

(comment
  (deftype EdgeWrap [w]
    Element
    (get-graph [_] [:edge w]))

  (deftype VertexWrap [w]
    Element
    (get-graph [_] [:vertex w]))

  (->VertexWrap nil)
  (type (->mV nil nil))

  (def w (wrapper VertexWrap ->VertexWrap EdgeWrap ->EdgeWrap)))


;; Support to add extensions:

(defmulti compile-wrapper (fn compile-wrapper-dispatch [method wrap-fn result-type] result-type))

(defmethod compile-wrapper :default [method wrap-fn result-type]
  (fn
    ([o] (let [W+ (-wrapper o)] (method (wrap-fn W+ o))))
    ([o a] (let [W+ (-wrapper o)] (method (wrap-fn W+ o) a)))
    ([o a b] (let [W+ (-wrapper o)] (method (wrap-fn W+ o) a b)))
    ([o a b & args] (let [W+ (-wrapper o)] (apply method (wrap-fn W+ o) a b args)))))

(defmethod compile-wrapper :forked-graph [method wrap-fn result-type]
  (fn
    ([o] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o)) (->mForkedGraph (forked-graph-wrapper W+)))))
    ([o a] (let [W+ (-wrapper o)] (some-> (->mForkedGraph (method (wrap-fn W+ o) a) (forked-graph-wrapper W+)))))
    ([o a b] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o) a b) (->mForkedGraph (forked-graph-wrapper W+)))))
    ([o a b & args] (let [W+ (-wrapper o)]
                      (some-> (apply method (wrap-fn W+ o) a b args) (->mForkedGraph (forked-graph-wrapper W+)))))))

(defmethod compile-wrapper :linear-graph [method wrap-fn result-type]
  (fn
    ([o] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o)) (->mLinearGraph (linear-graph-wrapper W+)))))
    ([o a] (let [W+ (-wrapper o)] (some-> (->mLinearGraph (method (wrap-fn W+ o) a) (linear-graph-wrapper W+)))))
    ([o a b] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o) a b) (->mLinearGraph (linear-graph-wrapper W+)))))
    ([o a b & args] (let [W+ (-wrapper o)]
                      (some-> (apply method (wrap-fn W+ o) a b args) (->mLinearGraph (linear-graph-wrapper W+)))))))

(defmethod compile-wrapper :vertex [method wrap-fn result-type]
  (fn
    ([o] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o)) (->mV (vertex-wrapper W+)))))
    ([o a] (let [W+ (-wrapper o)] (some-> (->mV (method (wrap-fn W+ o) a) (vertex-wrapper W+)))))
    ([o a b] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o) a b) (->mV (vertex-wrapper W+)))))
    ([o a b & args] (let [W+ (-wrapper o)]
                         (some-> (apply method (wrap-fn W+ o) a b args) (->mV (vertex-wrapper W+)))))))

(defmethod compile-wrapper [:vertex] [method wrap-fn result-type]
  (fn
    ([o]
     (let [W+ (-wrapper o)
           V+ (vertex-wrapper W+)]
       (map #(when % (->mV % V+))
            (method (wrap-fn W+ o)))))
    ([o a]
     (let [W+ (-wrapper o)
           V+ (vertex-wrapper W+)]
       (map #(when % (->mV % V+))
            (method (wrap-fn W+ o) a))))
    ([o a b]
     (let [W+ (-wrapper o)
           V+ (vertex-wrapper W+)]
       (map #(when % (->mV % V+))
            (method (wrap-fn W+ o) a b))))
    ([o a b & args]
     (let [W+ (-wrapper o)
           V+ (vertex-wrapper W+)]
       (map #(when % (->mV % V+))
            (apply method (wrap-fn W+ o) a b args))))))

(defmethod compile-wrapper :edge [method wrap-fn result-type]
  (fn
    ([o] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o)) (->mE (edge-wrapper W+)))))
    ([o a] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o) a) (->mE (edge-wrapper W+)))))
    ([o a b] (let [W+ (-wrapper o)] (some-> (method (wrap-fn W+ o) a b) (->mE (edge-wrapper W+)))))
    ([o a b & args] (let [W+ (-wrapper o)]
                         (some-> (apply method (wrap-fn W+ o) a b args) (->mE (edge-wrapper W+)))))))

(defmethod compile-wrapper [:edge] [method wrap-fn result-type]
  (fn
    ([o]
     (let [W+ (-wrapper o)
           E+ (edge-wrapper W+)]
       (map #(when % (->mE % E+))
            (method (wrap-fn W+ o)))))
    ([o a]
     (let [W+ (-wrapper o)
           E+ (edge-wrapper W+)]
       (map #(when % (->mE % E+))
            (method (wrap-fn W+ o) a))))
    ([o a b]
     (let [W+ (-wrapper o)
           E+ (edge-wrapper W+)]
       (map #(when % (->mE % E+))
            (method (wrap-fn W+ o) a b))))
    ([o a b & args]
     (let [W+ (-wrapper o)
           E+ (edge-wrapper W+)]
       (map #(when % (->mE % E+))
            (apply method (wrap-fn W+ o) a b args))))))


(defn- extend-wrapped [type protocol method-specs]
  (let [wrapped-impls (reduce (fn [wrapped-impls [method result-type]]
                                (let [compiled (compile-wrapper (ns-resolve (.ns (:var protocol)) (symbol (name method)))
                                                                (wrap-fn* protocol)
                                                                result-type)]
                                  (assoc wrapped-impls method compiled)))
                              ;; (:sigs protocol) have a bunch of stuff on the key but should just dispatch to :default
                              {} (merge (reduce-kv (fn [m k v] (assoc m k nil))
                                                   {} (:method-map protocol))
                                        method-specs))]
    (extend type
      protocol
      wrapped-impls)))

;; Add extensions

(defn extend-vertex
  "Extend the custom vertex type with the given protocol and method specs.

  Methods are generated from the specs via the `compile-wrapper` multimethod, so additional plain extension types may be added.

  method-specs "
  [protocol method-specs]
  (extend-wrapped mV protocol method-specs))

(defn extend-edge [protocol method-specs]
  (extend-wrapped mE protocol method-specs))

(defn extend-forked-graph [protocol method-specs]
  (extend-wrapped mForkedGraph protocol method-specs))

(defn extend-linear-graph [protocol method-specs]
  (extend-wrapped mLinearGraph protocol method-specs))
