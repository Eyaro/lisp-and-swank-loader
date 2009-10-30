(ns utils.symbol
 (:use utils.core))
(defn sym-to-key [sym] (if (keyword? sym) sym (read-string (str ":" sym))))

(defn sym-equals [sym1 sym2] (.equalsIgnoreCase (str sym1) (str sym2)))

(defnl sym-subseq [sym start &op end nil]
  (if end
    (read-string (subs (str sym) start end))
    (read-string (subs (str sym) start))))

(defn sym-at [sym num] (sym-subseq sym num (inc num)))




