(ns fermor.descend)

(defn- ensure-seq [x]
  ;; copied from core
  (if (or (nil? x) (sequential? x))
    x
    [x]))

(deftype Cons [first tail])
(deftype Concat [head tail])
(defrecord NoResult [path ^:long depth])

(defmethod print-method Cons [^Cons v ^java.io.Writer w]
  (.write w "(Cons ")
  (print-method (.first v) w)
  (.write w ", ")
  (if (.tail v)
    (.write w (str (class (.tail v))))
    (.write w "nil"))
  (.write w ")"))

(defmethod print-method Concat [^Concat v ^java.io.Writer w]
  (.write w "(Concat ")
  (if (.head v)
    (.write w (str (class (.head v))))
    (.write w "nil"))
  (.write w ", ")
  (if (.tail v)
    (.write w (str (class (.tail v))))
    (.write w "nil"))
  (.write w ")"))

;; Must be used together with extrude to work around limitation in Clojure lazy-seq concat
(defn *descend
  ([path f coll]
   (when-let [[e & more] (seq coll)]
     #(Cons. e
             (Concat. (*descend (when path (conj path e)) f (ensure-seq (f path e)))
                      (when more (*descend path f more))))))
  ([path control f coll ^:long nri ^:long recur-depth] ; nri is no-results-interval
   (when-let [[e & more] (seq coll)]
     (let [[emit children siblings reset-path] (control path e)
           results (when children
                     (ensure-seq (f path e)))
           path (when-not (nil? path) (if reset-path [] path))]

       (case [(boolean emit) (boolean children) (boolean siblings)]

         [true true true] ; (:emit-and-loop true :loop-and-emit)
         #(Cons. e (Concat. (*descend (when path (conj path e)) control f results nri 0)
                            (*descend path control f more nri 0)))

         [true false true] ; :emit
         #(Cons. e (*descend path control f more nri 0))

         [true true false] ; (:emit-and-chain :chain-and-emit)
         #(Cons. e (*descend (when path (conj path e)) control f results nri 0))

         [true false false] ; (:emit-and-cut :cut-and-emit)
         #(Cons. e nil)

         [false true true] ; :loop
         (let [more #(Concat. (*descend (when path (conj path e)) control f results nri (inc recur-depth))
                              (*descend path control f more nri (inc recur-depth)))]
           (if (= nri (mod recur-depth (inc nri)))
             #(Cons. (NoResult. path recur-depth) more)
             more))

         [false true false] ; :chain
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descend (when path (conj path e)) control f results nri (inc recur-depth)))
           (recur (when path (conj path e)) control f results nri (inc recur-depth)))

         [false false true] ; (:ignore false nil)
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descend path control f more nri (inc recur-depth)))
           (recur path control f more nri (inc recur-depth)))

         [false false false]; :cut
         nil)))))

;; Must be used together with extrude to work around limitation in Clojure lazy-seq concat
(defn *descents
  ([path f coll]
   (when-let [[e & more] (seq coll)]
     #(let [e-path (conj path e)]
        (Cons. e-path
               (Concat. (*descents e-path f (ensure-seq (f path e)))
                        (when more (*descents path f more)))))))
  ([path control f coll ^:long nri ^:long recur-depth] ; nri is no-results-interval
   (when-let [[e & more] (seq coll)]
     (let [[emit children siblings reset-path] (control path e)
           results (when children
                     (ensure-seq (f path e)))
           path (if reset-path [] path)
           e-path (conj path e)]

       (case [(boolean emit) (boolean children) (boolean siblings)]

         [true true true] ; (:emit-and-loop true :loop-and-emit)
         #(Cons. e-path (Concat. (*descents e-path control f results nri 0)
                                 (*descents path control f more nri 0)))

         [true false true] ; :emit
         #(Cons. e-path (*descents path control f more nri 0))

         [true true false] ; (:emit-and-chain :chain-and-emit)
         #(Cons. e-path (*descents e-path control f results nri 0))

         [true false false] ; (:emit-and-cut :cut-and-emit)
         #(Cons. e-path nil)

         [false true true] ; :loop
         (let [more #(Concat. (*descents e-path control f results nri (inc recur-depth))
                              (*descents path control f more nri (inc recur-depth)))]
           (if (= nri (mod recur-depth (inc nri)))
             #(Cons. (NoResult. path recur-depth) more)
             more))

         [false true false] ; :chain
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descents e-path control f results nri (inc recur-depth)))
           (recur e-path control f results nri (inc recur-depth)))

         [false false true] ; (:ignore false nil)
         (if (= nri (mod recur-depth (inc nri)))
           #(Cons. (NoResult. path recur-depth)
                   (*descents path control f more nri (inc recur-depth)))
           (recur path control f more nri (inc recur-depth)))

         [false false false] ; :cut
         nil)))))


(defn cut-no-results
  "A possible resolution to be used by a custom *no-results* function. Stop searching the current element and its siblings."
  []
  nil)

(defn continue-no-results
  "A possible resolution to be used by a custom *no-results* function. For
   instance to continue searching siblings but not children, call
   (continue-no-results nil right), or to search children but not siblings, call
   (continue-no-results down nil)."
  [down right]
  (Concat. down right))

(defn value-for-no-results
  "A possible resolution for *no-results*. Insert a constant value into the
   result stream and then continue searching."
  [chk-buffer value down right]
  (chunk-append chk-buffer value)
  (Concat. down right))

(defn ->no-results
  "This function is used to capture the value of *cut-no-results* so that it is
   not lost when the seq it is used in is returned from the binding context."
  [^:long cut-no-results]
  (fn [chk-buffer no-result down right]
    (if (< (:depth no-result) cut-no-results)
      (continue-no-results down right)
      (continue-no-results nil right))))

(defn- *extrude [b x fno-results tails]
  ; b is always created in the calling method. It is mutable and does not need to be returned.
  (let [x (if (fn? x) (x) x)]
    (if x
      (cond (instance? Cons x)
            (let [^Cons c x]
              (let [v (.first c)]
                (if (instance? NoResult v)
                  (fno-results b v (.tail c) tails)
                  (do (chunk-append b (.first c))
                      (if (= 32 (count b))
                        (if tails
                          (Concat. (.tail c) tails)
                          (.tail c))
                        (recur b (.tail c) fno-results tails))))))
            (instance? Concat x)
            (let [^Concat c x]
              (let [head (.head c)
                    tail (.tail c)]
                (cond head
                      (recur b head fno-results (if tail
                                                  (if tails
                                                    (Concat. tail tails)
                                                    tail)
                                                  tails))
                      tail
                      (recur b tail fno-results tails)
                      :else
                      tails)))
            :else
            tails)
      tails)))

(defonce ^:dynamic ^:long *cut-no-results* 1000000)
(defonce ^:dynamic ^:long *no-result-interval* 10000)
(defonce ^:dynamic *no-results* nil)

(defn extrude
  ([x]
   (extrude x (or *no-results* (->no-results *cut-no-results*))))
  ([x no-results]
   (lazy-seq
    (let [b (chunk-buffer 32)
          conc (*extrude b x no-results nil)]
      (if conc
        (chunk-cons (chunk b) (extrude conc no-results))
        (chunk-cons (chunk b) nil))))))
