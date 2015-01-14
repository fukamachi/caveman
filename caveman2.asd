#|
  This file is a part of caveman project.
  Copyright (c) Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Lightweight web application framework

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2-asd
  (:use :cl :asdf))
(in-package :caveman2-asd)

(defsystem caveman2
  :version "2.3.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:ningle
               :cl-project
               :dbi
               :cl-syntax-annot
               :myway)
  :components ((:module "v2/src"
                :components
                ((:file "caveman" :depends-on ("app" "route" "helper" "skeleton"))
                 (:file "app" :depends-on ("exception"))
                 (:file "route" :depends-on ("app" "nested-parameter"))
                 (:file "nested-parameter")
                 (:file "helper" :depends-on ("app"))
                 (:file "exception")
                 (:file "skeleton"))))
  :description "Lightweight web application framework"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.v2.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op caveman2-test))))
