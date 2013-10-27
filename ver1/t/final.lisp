(clack.util:namespace caveman-test.final
  (:use :cl
        :caveman
        :cl-test-more))

(plan 0)

(diag "myapp stop")
(funcall (intern "STOP" :myapp))

(cl-fad:delete-directory-and-files (asdf:component-pathname (asdf:find-system :myapp)))
(finalize)
