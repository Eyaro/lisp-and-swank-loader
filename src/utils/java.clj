(in-ns 'utils.package)
(import '(java.lang ProcessBuilder Thread ThreadGroup))
(use '[clojure.contrib.pprint :only (cl-format)])

(defn jprint [st & items]
  (if (string? st)
    (.print System/out (apply cl-format nil st items))
    (.print System/out (apply str st items))))

(defn jprintln [st & items]
  (if (string? st)
    (.println System/out (apply cl-format nil st items))
    (.println System/out (apply str st items))))

(defn new-process-builder [executable args env]
  (let [builder (ProcessBuilder. `(~executable ~@args))]
    (doseq [env (group env 2)]
      (.. builder (environment) (put (first env) (second env))))
    builder))



(defmacro with-thread [nm  & body]
  (let [[thread-name thread-group catch-it] (mkseq nm)
        thread-gen (gensym)]
    `(let [thread# (Thread.
                     ~@(if thread-group (list thread-group))
                     (fn []  ~(if catch-it
                                `(try
                                   (do ~@body)
                                   (catch  java.lang.InterruptedException ~thread-gen
                                     ~(if-not (true? catch-it)
                                        `(~catch-it ~thread-gen))))
                                `(do ~@body))))]
       ~(if thread-name `(.setName thread# ~thread-name))
       (.start thread#)
       thread#)))

(defmacro with-global-thread [nm global & body]
  (let [globals (mapcat list (repeatedly gensym) global)]
    `(let [~@globals]
       (with-thread ~nm
         (binding [~@(reverse globals)]
           ~@body)))))




(defn write-bytes [output-stream string args]
   (.flush output-stream)
   (.writeBytes output-stream (apply cl-format nil (str string) args))
   (.flush output-stream))

(defn current-thread []
 (Thread/currentThread))
(defn new-group-thread [nm]
  (ThreadGroup. nm))

(defn current-thread-group []
  (.getThreadGroup (current-thread)))


