(defsystem "caveman2-db"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("caveman-middleware-dbimanager"
               "dbi"
               "sxql")
  :components ((:file "v2/src/db"))
  :description "Simple CL-DBI wrapper")
