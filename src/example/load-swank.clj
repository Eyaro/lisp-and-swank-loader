(ns example.load-swank
 (:use swank.loader lisp.loader))
(comment
  (defn start-lispin []
    (let [lisp (new-lisp :sbcl
                 "C:/Program Files/Steel Bank Common Lisp/1.0.29/sbcl.exe"
                 '("--core" "C:/Program Files/Steel Bank Common Lisp/1.0.29/asdf/sbcl.core")
                 '("SBCL_HOME" "C:/Program Files/Steel Bank Common Lisp/1.0.29/"))
          swank (new-swank :sbcl "(load \"C:/Seth/slime/swank-loader.lisp\")" 4040)]
      (launch lisp swank)))
  

  (start-lispin)
  )


