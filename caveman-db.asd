(in-package :cl-user)
(defpackage caveman-db-asd
  (:use :cl :asdf))
(in-package :caveman-db-asd)

(defsystem caveman-db
  :version "0.1"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:caveman-middleware-dbimanager
               :dbi
               :sxql)
  :components ((:file "src/db"))
  :description "Simple CL-DBI wrapper")
