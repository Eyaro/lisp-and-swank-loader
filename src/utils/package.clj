(ns utils.package
 (:use
   [clojure.contrib.def :only (defmacro-)]))

(defmacro load-files [& files]
  `(do ~@(map (fn [file] `(load ~file)) files)))

(load-files
  "macros"
     "symbol"
     "sequence"
          "java"
  "string"
       "nio"
  )



