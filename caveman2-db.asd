(in-package :cl-user)
(defpackage caveman2-db-asd
  (:use :cl :asdf))
(in-package :caveman2-db-asd)

(defsystem caveman2-db
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:caveman-middleware-dbimanager
               :dbi
               :sxql)
  :components ((:file "v2/src/db"))
  :description "Simple CL-DBI wrapper")
