#|
  This file is a part of caveman project.
  Copyright (c) Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "caveman2-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("caveman2"
               "clack-handler-hunchentoot"
               "lack-component"
               "uiop"
               "usocket"
               "dexador"
               "prove"
               "trivial-types")
  :components ((:module "v2/t"
                :serial t
                :components
                ((:test-file "caveman")
                 (:test-file "route")
                 (:test-file "nested-parameter"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
