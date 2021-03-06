(ns swank.loader
 (:use
   utils.core utils.java utils.nio  utils.sequence
   lisp.loader
   clojure.contrib.def)
 (:require [clojure.contrib.str-utils2 :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BACKBONE
(defvar- *lisp* nil) 
(defvar- *swank* nil)
(defvar- *port* 4040)
(defstruct swank :port :swank-load-cmd :type)

(defmulti new-swank (fn [& args] (first args)))
(defmethodl new-swank :default [lisp-type swank-load-cmd
                                &op port *port*]
  (struct-map swank :type lisp-type :port port :swank-load-cmd swank-load-cmd))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;REDIRECTION OF LISP STREAM
(load "redirect")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SWANK UTILITIES UTILITIES


(defn into-lisp [s & args]
  (write-lisp *lisp* s args))

(defn load-swank []
  (when (:swank-load-cmd *swank*)
    (into-lisp (:swank-load-cmd *swank*)))
  (into-lisp
    "(swank-loader::init :load-contribs t :setup t)")
  (into-lisp
    "(swank:create-server :coding-system \"utf-8\" :port ~A)~%"
    (:port *swank*)))

;;;;;;LAUNCHING
(defmacro with-lisp [[lisp swank] & body]
  `(let [lisp# ~lisp swank# ~swank]
     (binding [~'*lisp* lisp# ~'*swank* swank#] ~@body)))

(defn join-lisp [lisp]
  (.join @(:error-thread lisp)))
  
(defn launch [lisp swank]
  (with-lisp ((start-lisp lisp) swank) 
    (let [thread-group (startup-lisp-monitor monitor-lisp-stream)]
      (load-swank)
      (merge *lisp* {:worker-threads thread-group}))))




