(ns example.load-swank
 (:use 
   swank.loader lisp.loader
   client.swank))

(defn start-lispin []
  (let [lisp (new-lisp :sbcl
               "C:/Program Files/Steel Bank Common Lisp/1.0.29/sbcl.exe"
               '("--core" "C:/Program Files/Steel Bank Common Lisp/1.0.29/asdf/sbcl.core")
               '("SBCL_HOME" "C:/Program Files/Steel Bank Common Lisp/1.0.29/"))
        swank (new-swank :sbcl "(load \"C:/Seth/slime/swank-loader.lisp\")" 4040)]
    (launch lisp swank)))

(defclient client-decide [event conn]
  (try
    (println event)
    (catch Exception e 
      (println "EXCEPTION"))))

(comment
  (def *lisp* (start-lispin))
  ;;you have to wait until lisp is loaded until you do the following...
  (join-lisp *lisp*)
  (def *client*  
    (start-client
      nil
      #'client-decide
      :host "127.0.0.1" :port 4040)) 
  (write-to-connection *client* "(:emacs-rexs '(print 'hi))")
  )
