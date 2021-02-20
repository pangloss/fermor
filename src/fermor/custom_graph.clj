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
  (wrapper-type ^Class [w])
  (constructor [w]))

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
  (wrapper-type ^Class [w] t)
  ;; this is wrong
  (constructor [w] ->))

(defn wrap-graph [graph ->F ->L ->V ->E]
  ;; default type to Object which won't implement the desired interfaces so will
  ;; never trigger use of the constructor, without requiring any other special
  ;; conditional logic.
  (let [WrappedForkedGraphType (if ->F (type (->F nil)) Object)
        WrappedLinearGraphType (if ->L (type (->L nil)) Object)
        WrappedVertexType (if ->V (type (->V nil)) Object)
        WrappedEdgeType (if ->E (type (->E nil)) Object)
        F+ (Wrapper. WrappedForkedGraphType ->F nil nil nil nil)
        L+ (Wrapper. WrappedLinearGraphType ->L nil nil nil nil)
        E+ (Wrapper. WrappedEdgeType ->E nil nil nil nil)
        V+ (Wrapper. WrappedVertexType ->V nil nil nil nil)]
    (doseq [[setter value] [[set-E! E+] [set-V! V+] [set-F! F+] [set-L! L+]]
            target [E+ V+ F+ L+]]
       (setter target value))
    (condp satisfies? graph
      Forked (->mForkedGraph graph F+)
      Linear graph ;; (->mLinearGraph graph L+)
      Vertex (->mV graph V+)
      Edge (->mE graph E+))))

(defmacro wrap-fn
  ([name interface]
   `(defn ~name [wrapper# element#]
      (if (.isAssignableFrom ~interface (wrapper-type wrapper#))
        ((constructor wrapper#) element#)
        element#)))
  ([name interface mini-wrapper]
   `(defn ~name [wrapper# element#]
      (if (.isAssignableFrom ~interface (wrapper-type wrapper#))
        (mini-wrapper element#)
        element#))))

(defmacro wrap-inline [name interface]
  `(defmacro ~name [wrapper# element#]
     `(if (.isAssignableFrom  ~~interface (wrapper-type ~wrapper#))
        ((constructor ~wrapper#) ~element#)
        ~element#)))

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
  (element-id [v] (-> V+ (->element-id element) element-id))
  (get-graph [v] (-> V+ (->get-graph element) get-graph))
  (get-document [v] (-> V+ (->get-document element) get-document))
  (get-document [v key] (-> V+ (->get-document element) (get-document key)))

  Wrappable
  (-unwrap [e] (-unwrap element))

  Wrapping
  (plain [w] element)
  (-wrapper [w] V+)

  Vertex
  (-out-edges [v]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex element) -out-edges))))
  (-out-edges [v labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex element) (-out-edges labels)))))
  (-out-edges [v _ prepared-labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex element) (-out-edges _ prepared-labels)))))
  (-in-edges [v]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex element) -in-edges))))
  (-in-edges [v labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex element) (-in-edges labels)))))
  (-in-edges [v _ prepared-labels]
    (let [E+ (edge-wrapper V+)]
      (map #(->mE % E+)
           (-> V+ (->vertex element) (-in-edges _ prepared-labels))))))

(deftype mE [element E+]
  Object
  (equals [a b] (.equals element b))
  (hashCode [e] (.hashCode element))

  Element
  (element-id [v] (-> E+ (->element-id element) element-id))
  (get-graph [e] (-> E+ (->get-graph element) get-graph))
  (get-document [e] (-> E+ (->get-document element) get-document))

  Wrappable
  (-unwrap [e] (-unwrap element))

  Wrapping
  (plain [w] element)
  (-wrapper [w] E+)

  WeightedEdge
  (-weight [e] (-> E+ (->weight element) -weight))

  Edge
  (-label [e] (-> E+ (->label element) -label))
  (in-vertex [e] (-> E+ (->edge element) in-vertex (->mV (vertex-wrapper E+))))
  (out-vertex [e] (-> E+ (->edge element) out-vertex (->mV (vertex-wrapper E+)))))

(deftype mForkedGraph [graph F+]
  Object
  (equals [a b] (.equals graph b))
  (hashCode [e] (.hashCode graph))

  Graph
  (all-vertices [g]
    (let [V+ (vertex-wrapper F+)]
      (map #(->mV % V+) (-> F+ (->all-vertices graph) all-vertices))))
  (all-vertices [g kind]
    (when-let [of-kind (->of-kind F+)]
      (->> (all-vertices g)
           (of-kind kind))))
  (get-vertex [g id]
    (-> F+ (->get-vertex graph) (get-vertex id) (->mV (vertex-wrapper F+))))

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
  (print-method (-unwrap e) w))

(defmethod simple-dispatch mE [o]
  (print-method o *out*))

(defmethod print-method mV [e ^java.io.Writer w]
  (.write w "(V ")
  (print-method (:kind (element-id e)) w)
  (.write w " ")
  (print-method (:id (element-id e)) w)
  (when-not *compact-vertex-printing*
    (when-let [p (get-document e)]
      (.write w " ")
      (print-method p w)))
  (.write w ")"))

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
                                (assoc wrapped-impls method (compile-wrapper method arity wrap-fn result-type)))
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

;; Example


;; NOTE: this must be the interface that backs the protocol
(wrap-fn ->kind fermor.protocols.Kind)

(deftype VKind [vertex]
  Kind
  (kind [v] (:kind (element-id vertex))))

(extend-vertex Kind
               {:method kind :arity 1 :wrap-fn ->kind})
