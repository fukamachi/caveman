#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman-asd
  (:use :cl :asdf))
(in-package :caveman-asd)

(defsystem caveman
  :version "12.08.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack-v1-compat
               :myway
               :anaphora
               :cl-ppcre
               :cl-syntax
               :cl-syntax-annot
               :cl-emb
               :local-time
               :cl-project
               :do-urlencode)
  :components ((:module "v1/src"
                :components
                ((:module "core"
                  :components
                  ((:file "caveman" :depends-on ("route" "context" "app" "project"))
                   (:file "app" :depends-on ("request" "middleware/context"))
                   (:file "project" :depends-on ("context" "middleware/context"))
                   (:file "request")
                   (:file "response")
                   (:file "context" :depends-on ("request" "response"))
                   (:file "middleware/context" :depends-on ("context"))
                   (:file "skeleton")
                   (:file "route" :depends-on ("app"))
                   (:file "widget")))
                 (:module "lib"
                  :depends-on ("core")
                  :components
                  ((:module "widget"
                    :components
                    ((:file "form")))
                   (:module "view"
                    :serial t
                    :components
                    ((:file "function")
                     (:file "emb"))))))))
  :description "Web Application Framework for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op caveman-test))))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :caveman))))
  (warn "\"caveman\" is deprecated now. Did you mean \"caveman2\"?"))
