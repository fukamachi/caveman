(in-package :cl-user)
(defpackage caveman-test.final
  (:use :cl
        :caveman
        :cl-test-more))
(in-package :caveman-test.final)

(plan 0)

(diag "myapp stop")
(funcall (intern "STOP" :myapp))


(uiop:delete-directory-tree (asdf:component-pathname (asdf:find-system :myapp))
                            :validate t :if-does-not-exist :ignore)
(finalize)
