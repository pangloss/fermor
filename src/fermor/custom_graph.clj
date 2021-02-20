(ns fermor.custom-graph
  (:use fermor.protocols)
  (:require [clojure.pprint :refer [simple-dispatch]]))

(defprotocol IWrapper
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

(deftype Wrapper [t -> ^:unsynchronized-mutable F ^:unsynchronized-mutable L ^:unsynchronized-mutable V ^:unsynchronized-mutable E]
  IWrapper
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

(defn wrap-graph [graph ->FImpl ->LImpl ->VImpl ->EImpl]
  ;; default type to Object which won't implement the desired interfaces so will
  ;; never trigger use of the make-implementor, without requiring any other special
  ;; conditional logic.
  (let [ForkedGraphImplType (if ->FImpl (type (->FImpl nil)) Object)
        LinearGraphImplType (if ->LImpl (type (->LImpl nil)) Object)
        VertexImplType (if ->VImpl (type (->VImpl nil)) Object)
        EdgeImplType (if ->EImpl (type (->EImpl nil)) Object)
        F+ (Wrapper. ForkedGraphImplType ->FImpl nil nil nil nil)
        L+ (Wrapper. LinearGraphImplType ->LImpl nil nil nil nil)
        E+ (Wrapper. EdgeImplType ->EImpl nil nil nil nil)
        V+ (Wrapper. VertexImplType ->VImpl nil nil nil nil)]
    (doseq [[setter value] [[set-E! E+] [set-V! V+] [set-F! F+] [set-L! L+]]
            target [E+ V+ F+ L+]]
       (setter target value))
    (condp satisfies? graph
      Forked (->mForkedGraph graph F+)
      Linear graph ;; (->mLinearGraph graph L+)
      Vertex (->mV graph V+)
      Edge (->mE graph E+))))

(defmacro wrap-fn [name interface]
  (let [interface (if (map? interface)
                    (:on-interface interface) ;; get interface from protocol
                    interface)])
  `(defn ~name [wrapper# element#]
     (prn (.isAssignableFrom ~interface (implementor-type wrapper#)))
     (if (.isAssignableFrom ~interface (implementor-type wrapper#))
       ((make-implementor wrapper#) element#)
       (plain element#))))

(defmacro wrap-inline [name interface]
  `(defmacro ~name [wrapper# element#]
     `(if (.isAssignableFrom  ~~interface (implementor-type ~wrapper#))
        ((make-implementor ~wrapper#) ~element#)
        (plain ~element#))))

(do
  ;; NOTE: if any of these give problems, switch to wrap-fn to debug.
  (wrap-inline ->vertex fermor.protocols.Vertex)
  (wrap-inline ->element-id fermor.protocols.Element)
  (wrap-inline ->get-graph fermor.protocols.Element)
  (wrap-inline ->get-document fermor.protocols.Element)
  (wrap-inline ->weight fermor.protocols.WeightedEdge)
  (wrap-inline ->label fermor.protocols.Edge)
  (wrap-inline ->edge fermor.protocols.Edge)
  (wrap-inline ->all-vertices fermor.protocols.Graph)
  (wrap-inline ->get-vertex fermor.protocols.Graph))

(comment (macroexpand '(->vertex 'element 'V+)))


(defprotocol Wrapping
  (plain [w])
  (-wrapper [w]))

(declare ->mE)

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

  Vertex
  (-out-edges [v]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex v) -out-edges))))
  (-out-edges [v labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex v) (-out-edges labels)))))
  (-out-edges [v _ prepared-labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex v) (-out-edges _ prepared-labels)))))
  (-in-edges [v]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex v) -in-edges))))
  (-in-edges [v labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex v) (-in-edges labels)))))
  (-in-edges [v _ prepared-labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex v) (-in-edges _ prepared-labels))))))

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
  (all-vertices [g]
    (let [V+ (vertex-wrapper F+)]
      (map #(->mV % V+) (-> F+ (->all-vertices g) all-vertices))))
  (all-vertices [g kind]
    (when-let [of-kind (->of-kind F+)]
      (->> (all-vertices g)
           (of-kind kind))))
  (get-vertex [g id]
    (-> F+ (->get-vertex g) (get-vertex id) (->mV (vertex-wrapper F+))))

  Wrapping
  (plain [w] graph)
  (-wrapper [w] F+)

  Wrappable
  (-unwrap [g]
    (-unwrap graph)))

#_
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

  clojure.lang.IMeta
  (meta [o] (-> L+ (->meta graph) metadata))

  Wrapping
  (plain [w] graph)
  (-wrapper [w] L+)

  IEdgeGraphs
  (_getLabels ^clojure.lang.IPersistentVector [g]
    (vec (.keys edges)))
  (_getEdgeGraph ^IGraph [g label]
    (let [x (.get edges label)]
      (when-not (.isEmpty x)
        (.get x))))
  (_removeEdgeGraph ^ILabelGraphs [g label]
    (let [x (.get edges label)]
      (if (.isEmpty x)
        g
        (->LinearGraph (.remove edges label) documents settings nil))))
  (_addEdgeGraph ^ILabelGraphs [g label ^IGraph edge]
    (->LinearGraph (.put edges label edge) documents settings nil))

  Linear
  (to-forked [g]
    (->ForkedGraph (.forked (.mapValues edges
                                        (reify BiFunction
                                          (apply [this k v]
                                            (.forked ^IGraph v)))))
                   (.forked documents)
                   settings
                   metadata)))

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


;; Add extensions:

(defmulti compile-wrapper (fn compile-wrapper-dispatch [method arity wrap-fn result-type] result-type))

(defmethod compile-wrapper :default [method arity wrap-fn result-type]
  (case arity
    1 (fn [o] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) method)))
    2 (fn [o a] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) (method a))))
    3 (fn [o a b] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) (method a b))))))

(defmethod compile-wrapper :vertex [method arity wrap-fn result-type]
  (case arity
    1 (fn [o] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) method (->mV (vertex-wrapper W+)))))
    2 (fn [o a] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) (method a) (->mV (vertex-wrapper W+)))))
    3 (fn [o a b] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) (method a b) (->mV (vertex-wrapper W+)))))))

(defmethod compile-wrapper [:vertex] [method arity wrap-fn result-type]
  (case arity
    1 (fn [o]
        (let [W+ (-wrapper o)
              V+ (vertex-wrapper W+)]
          (map #(->mV % V+)
               (-> W+ (wrap-fn o) method))))
    2 (fn [o a]
        (let [W+ (-wrapper o)
              V+ (vertex-wrapper W+)]
          (map #(->mV % V+)
               (-> W+ (wrap-fn o) (method a)))))
    3 (fn [o a b]
        (let [W+ (-wrapper o)
              V+ (vertex-wrapper W+)]
          (map #(->mV % V+)
               (-> W+ (wrap-fn o) (method a b)))))))

(defmethod compile-wrapper :edge [method arity wrap-fn result-type]
  (case arity
    1 (fn [o] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) method (->mE (edge-wrapper W+)))))
    2 (fn [o a] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) (method a) (->mE (edge-wrapper W+)))))
    3 (fn [o a b] (let [W+ (-wrapper o)] (-> W+ (wrap-fn o) (method a b) (->mE (edge-wrapper W+)))))))

(defmethod compile-wrapper [:edge] [method arity wrap-fn result-type]
  (case arity
    1 (fn [o]
        (let [W+ (-wrapper o)
              E+ (edge-wrapper W+)]
          (map #(->mE % E+)
               (-> W+ (wrap-fn o) method))))
    2 (fn [o a]
        (let [W+ (-wrapper o)
              E+ (edge-wrapper W+)]
          (map #(->mE % E+)
               (-> W+ (wrap-fn o) (method a)))))
    3 (fn [o a b]
        (let [W+ (-wrapper o)
              E+ (edge-wrapper W+)]
          (map #(->mE % E+)
               (-> W+ (wrap-fn o) (method a b)))))))

(defn- extend-wrapped [type protocol method-specs]
  (let [wrapped-impls (reduce (fn [wrapped-impls {:keys [method arity wrap-fn result-type]}]
                                (assoc wrapped-impls method (compile-wrapper (resolve (symbol (name method))) arity wrap-fn result-type)))
                              {} method-specs)]
    (extend type
      protocol
      wrapped-impls)))

(defn extend-vertex
  "Extend the custom vertex type with the given protocol and method specs.

  Methods are generated from the specs via the `compile-wrapper` multimethod, so additional plain extension types may be added.

  method-specs "
  [protocol method-specs]
  (extend-wrapped mV protocol method-specs))

(defn extend-edge [protocol method-specs]
  (extend-wrapped mE protocol method-specs))
