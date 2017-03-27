#|
  This file is a part of caveman project.
  Copyright (c) Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Lightweight web application framework

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "caveman2"
  :version "2.4.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("ningle"
               "lack-request"
               "lack-response"
               "cl-project"
               "dbi"
               "cl-syntax-annot"
               "myway"
               "quri")
  :components ((:module "v2/src"
                :components
                ((:file "caveman" :depends-on ("app" "route" "helper" "skeleton"))
                 (:file "app" :depends-on ("exception"))
                 (:file "route" :depends-on ("app" "nested-parameter"))
                 (:file "nested-parameter")
                 (:file "helper" :depends-on ("app"))
                 (:file "exception")
                 (:file "skeleton"))))
  :description "Lightweight web application framework"
  :long-description #.(read-file-string (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "caveman2-test"))))
