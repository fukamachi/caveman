#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman-test-asd
  (:use :cl :asdf))
(in-package :caveman-test-asd)

(defclass asdf::test-file (asdf:cl-source-file) ())
(defmethod asdf:perform ((op asdf:load-op) (c asdf::test-file))
  ;; do nothing
  )
(defmethod asdf:perform ((op asdf:test-op) (c asdf::test-file))
  (asdf:perform (make-instance 'asdf:load-op)
   (change-class c 'asdf:cl-source-file)))

(defsystem caveman-test
  :depends-on (:caveman
               :cl-test-more
               :cl-fad
               :drakma
               :usocket)
  :components ((:module "t"
                :serial t
                :components
                ((:test-file "init")
                 (:test-file "caveman")
                 (:test-file "final")))))
