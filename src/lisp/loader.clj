(ns lisp.loader
 (:use
   utils.package)
 (:import (java.io DataOutputStream)))

(defstruct lisp-implementation
  :type
  :fatal-error
  :process-builder
  :process
  :output-stream ;;output stream is DataOutputStream type
  :input-stream :error-stream
  :input-thread :error-thread)

(defn- mk-lisp-structure [& args]  
  (apply struct-map lisp-implementation
    :input-thread (atom nil) :error-thread (atom nil)
    args))

(defn lisp-type [lisp & args] (:type lisp))

;;;;;;;;;;
(defn lisp-exit-code [lisp-implementation]
  (awhen (:process lisp-implementation)
    (.exitValue it)))

(defn kill-lisp [lisp-implementation]
  (awhen (:process lisp-implementation)
    (.destroy it)))

(defn write-lisp [lisp string args]
  (write-bytes (:output-stream lisp)  string args))

