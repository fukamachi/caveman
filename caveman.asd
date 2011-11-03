#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman-asd
  (:use :cl :asdf))
(in-package :caveman-asd)

(defsystem caveman
  :version "11.11-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :anaphora
               :cl-ppcre
               :cl-syntax
               :cl-syntax-annot
               :clsql
               :cl-fad
               :cl-emb
               :local-time
               :cl-project)
  :components ((:module "src"
                :components
                ((:module "core"
                  :components
                  ((:file "caveman" :depends-on ("route" "context" "app" "project"))
                   (:file "app" :depends-on ("request" "context"))
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
          seq))))
