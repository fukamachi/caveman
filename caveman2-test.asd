#|
  This file is a part of caveman project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2-test-asd
  (:use :cl :asdf))
(in-package :caveman2-test-asd)

(defsystem caveman2-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:caveman2
               :cl-fad
               :usocket
               :drakma
               :cl-test-more)
  :components ((:module "v2/t"
                :serial t
                :components
                ((:file "caveman")
                 (:file "route"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
