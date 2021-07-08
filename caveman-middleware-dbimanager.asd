(defsystem "caveman-middleware-dbimanager"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("dbi")
  :components ((:file "v2/src/middleware/dbimanager"))
  :description "Clack Middleware for managing CL-DBI connections")
