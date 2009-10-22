(in-ns 'utils.package)

(use '[clojure.contrib.def :only (name-with-attributes defnk)]
     '[clojure.contrib.cond :only (cond-let)])
     
;;;;THINGS that are not functions that are required in here. Are also in other files which
;;;;overwrite these
(defn group [source n]
  (if (zero? n) (pr "error: zero length"))
  (let [t (int (/ (count source) n))
        remainder (drop (* n t) source)]
    (lazy-cat
     (for [i (range t)]
       (take n (drop (* i n) source)))
     (if (empty? remainder)
       '()
       (list remainder)))))

(defn mkseq [item]
  (cond
    (symbol? item) (seq (list item))
    :default (seq item)))
;;;;;;;;;;;;;;;;

(defmacro prog1 [& body]
  " (prog1 forms*)
Evaluates all the forms, returning the result of the first form"
  `(let [result# ~(first body)]
     ~@(rest body)
     result#))

(defmacro abbrev [short long]
  `(defmacro ~short [& args#]
     `(~'~long ~@args#)))

(defmacro abbrevs [& names]
  `(do ~@(map (fn [pair] `(abbrev ~@pair)) names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OPTIONAL ARGUMENTS/KEY ARGUMENT COMBO FOR functions and methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sym-equals [sym1 sym2] (.equalsIgnoreCase (str sym1) (str sym2)))

(defn- parse-op [the-name all]
  (let [[new-name all] (name-with-attributes the-name all)
        args (first all)
        body (rest all)]
    (let [[pos *ops] (split-with #(not (sym-equals % '&op)) args)
          ops (group (rest *ops) 2)
          gen-names (map (fn [_] (gensym)) ops)
          default-ops (mapcat (fn [gen ops]
                                (list (first ops) `(or ~gen ~(second ops))))  gen-names ops)]
      `[[~@pos & rest#]
        (let [[~@gen-names] rest#]
          (let [~@default-ops]
            ~@body))
        ~new-name])))

(defmacro defnop [func-name & all]
  (let [[args body new-name] (parse-op func-name all)]
    `(defn ~new-name ~args ~body)))

(defn- arg-type [args]
  (let [result (first (filter #(or (sym-equals % '&op) (keyword? %)) args))]
    (cond
      (keyword? result) :key
      (or (nil? result) (sym-equals result '())) :regular
      (sym-equals result '&op) :optional)))


(defmacro defnl [the-name & all]
  (let [[the-name all]  (name-with-attributes the-name all)]
    (condp = (arg-type (first all))
      :regular `(defn ~the-name ~@all)
      :optional `(defnop ~the-name ~@all)
      :key `(defnk ~the-name ~@all))))

(defn- get-defnl-reg-args [args]
  (take-while #(not (or (keyword? %) (sym-equals % '&op))) args))


(defmacro defmethodl [method-name dispatch args & body]
  (let [defnl-reg-args (get-defnl-reg-args args) name-gen (gensym)]
    (cond
      (= (arg-type args) :regular) `(defmethod ~method-name ~dispatch ~args ~@body)
      :else
      `(do
         (defnl ~name-gen [~@args] ~@body)
         (defmethod ~method-name ~dispatch [~@defnl-reg-args & rest#]
           (apply ~name-gen ~@defnl-reg-args rest#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LISP COMPATABILITY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro llet [bind & body]
  `(let ~(vec
           (mapcat #(if (symbol? %) (list % nil) %) bind))
     ~@body))

(defmacro lcond [& cond]
  (let [bindings
        (mapcat
          #(list (first %) `(do ~@(rest %)))
          cond)]
    `(cond ~@bindings)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONDLET MACRO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defnl lassoc [item lst &op ttest =]
  (first (filter #(ttest (first (mkseq %)) item) lst)))

(defn- condlet-binds [vars cl]
  (map (fn [bindform]
         (if (list? bindform)
           (cons
             (second (lassoc (first bindform) vars))
             (rest bindform))))
    (rest cl)))

(defn- condlet-clauses [vars cl bodfn]
  `(~(first cl) (llet ~(map second vars)
                  (llet ~(condlet-binds vars cl)
                    (~bodfn ~@(map second vars))))))

(defmacro condlet [clauses & body]
  (let [vars (map #(list % (gensym))
               (distinct
                 (map first
                   (mapcat rest clauses))))
        bodfn (gensym)]
    `(letfn [(~bodfn ~(vec (map first vars))
               ~@body)]
       (lcond ~@(map #(condlet-clauses vars  % bodfn)
                  clauses)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ANAPHORIC MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aif [test & [then else]]
  `(let [~'it ~test]
     (if ~'it ~then ~else)))

(defmacro awhen [test & body]
  `(aif ~test (do ~@body)))

(defmacro acond [& binds]
  (let [new-binds (mapcat (fn [b] `(~(first b) (do ~@(rest b)))) binds)]
    `(cond-let (~'it) ~@new-binds)))

(defmacro acondlet [clauses & body]
  (let [vars (map #(list % (gensym))
               (distinct
                 (map first
                   (mapcat rest clauses))))
        bodfn (gensym)]
    `(letfn ((~bodfn ~(vec (map first vars))
               ~@body))
       (acond ~@(map #(condlet-clauses vars  % bodfn)
                  clauses)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


