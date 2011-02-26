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
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :ponzu.db
               :cl-annot
               :cl-ppcre
               :clsql
               :cl-markup)
  :components ((:module "src"
                :components
                ((:file "caveman" :depends-on ("route" "model"))
                 (:file "route" :depends-on ("view"))
                 (:file "view")
                 (:file "layout" :depends-on ("caveman" "view"))
                 (:module "model"
                  :components
                  ((:file "model" :depends-on ("record" "table"))
                   (:file "table" :depends-on ("record"))
                   (:file "record")))))))
