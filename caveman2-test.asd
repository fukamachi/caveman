#|
  This file is a part of caveman project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2-test-asd
  (:use :cl :asdf))
(in-package :caveman-test-asd)

(defsystem caveman2-test
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:caveman
               :cl-test-more)
  :components ((:module "v2/t"
                :components
                ((:file "caveman"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
