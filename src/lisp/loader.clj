(ns lisp.loader
 (:import (java.io DataOutputStream))
 (:use utils.java utils.core))
;;;
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

;;;;;;;;;;
(defn lisp-exit-code [lisp-implementation]
  (awhen (:process lisp-implementation)
    (.exitValue it)))

(defn kill-lisp [lisp-implementation]
  (awhen (:process lisp-implementation)
    (.destroy it))
  (awhen (:worker-threads lisp-implementation)
    (if (instance? ThreadGroup it)
      (.interrupt it))))

(defn write-lisp [lisp string args]
  (write-bytes (:output-stream lisp)  string args))


;;;;;;;;;;;;MULTIMETHODS
;;;;;;;;;;;;
(defn lisp-type [lisp & args] (:type lisp))

(defmulti find-fatal-error lisp-type)
(defmethod find-fatal-error :default [lisp st]
  ;;inefficient to call again and again but it doesnt really matter....
  ;;TODO: make more efficient
  (re-find (re-pattern (apply str
                         (:fatal-error lisp) "|"
                         (:debugger-fatal-error lisp))) st))

(defmulti new-lisp (fn [& args] (first args)))
(defn- make-default-new-lisp [lisp-type exe args env]
  (mk-lisp-structure
    :type lisp-type
    :fatal-error "fatal\\s+error" 
    :debugger-fatal-error "debugger\\s+invoked\\s+on"
    :process-builder (new-process-builder exe args env)))

(defmethod new-lisp  :default [lisp-type exe args env]
  (make-default-new-lisp lisp-type exe args env))
(defmethod new-lisp :sbcl [lisp-type exe args env]
  (merge
    (make-default-new-lisp lisp-type exe args env)
    {:fatal-error "fatal\\s+error"
     :debugger-fatal-error "debugger\\s+invoked\\s+on"}))

(defmulti start-lisp lisp-type)
(defmethod start-lisp :default [lisp-implementation]
  (let [process (.start (:process-builder lisp-implementation))]
    (merge lisp-implementation {:process process
                                :output-stream (DataOutputStream. (.getOutputStream process))
                                :input-stream (agent (.getInputStream process))
                                :error-stream (agent (.getErrorStream process))})))

;;;;;;;;;;;
;;;;;;;;;;