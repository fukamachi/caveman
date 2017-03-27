#|
  This file is a part of caveman project.
  Copyright (c) Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "caveman2-test"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("caveman2"
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

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (op c) (symbol-call :prove.asdf :run-test-system c)))
