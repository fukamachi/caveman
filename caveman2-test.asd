#|
  This file is a part of caveman project.
  Copyright (c) Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2-test-asd
  (:use :cl :asdf))
(in-package :caveman2-test-asd)

(defsystem caveman2-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:caveman2
               :cl-fad
               :usocket
               :drakma
               :prove
               :trivial-types)
  :components ((:module "v2/t"
                :serial t
                :components
                ((:test-file "caveman")
                 (:test-file "route")
                 (:test-file "nested-parameter"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf)
                             c)))
