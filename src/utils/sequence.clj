(ns utils.sequence
  (:use utils.core))

;;from on-lisp->clojure converting project/blog
(defn- sym-to-key [sym] (if (keyword? sym) sym (read-string (str ":" sym))))
(defn mkseq [coll] (if (coll? coll) (seq coll) (seq (list coll))))

(defn single? [coll] (empty? (rest coll)))

(defmacro with-hash-keys [*keys *hash & body]
  (let
    [hash-keys (map #(sym-to-key
                       (if (coll? %)
                         (second %) %))
                 *keys)
     var-names (map #(if (coll? %)
                       (first %) %) *keys)]
    `(let [~(apply hash-map (interleave var-names hash-keys)) ~*hash]
       ~@body)))

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

(defnl lassoc [item lst &op ttest =]
  (first (filter #(ttest (first (mkseq %)) item) lst)))
