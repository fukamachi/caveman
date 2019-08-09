#|
  This file is a part of caveman project.
  Copyright (c) Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Clack Middleware for managing CL-DBI connections

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "caveman-middleware-dbimanager"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("clack-v1-compat"
               "dbi")
  :components ((:file "v2/src/middleware/dbimanager"))
  :description "Clack Middleware for managing CL-DBI connections")
