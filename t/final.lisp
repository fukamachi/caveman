(clack.util:namespace caveman-test.final
  (:use :cl
        :caveman
        :cl-test-more))

(plan 1)

(diag "myapp stop")
(funcall (intern "STOP" :myapp))

(is (symbol-value (intern "*ACCEPTOR*" :myapp))
    nil
    "finalized")

(cl-fad:delete-directory-and-files (asdf:component-pathname (asdf:find-system :myapp)))
(finalize)
