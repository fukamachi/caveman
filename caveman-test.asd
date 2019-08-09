#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(defsystem "caveman-test"
  :depends-on ("caveman"
               "cl-test-more"
               "uiop"
               "dexador"
               "usocket")
  :components ((:module "v1/t"
                :serial t
                :components
                ((:test-file "init")
                 (:test-file "caveman")
                 (:test-file "final"))))
  :defsystem-depends-on ("cl-test-more")
  :perform (test-op (o c) (symbol-call :cl-test-more :run-test-system c)))
