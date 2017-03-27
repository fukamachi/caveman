#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(defsystem "caveman"
  :version "12.08.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("clack-v1-compat"
               "myway"
               "anaphora"
               "cl-ppcre"
               "cl-syntax"
               "cl-syntax-annot"
               "cl-emb"
               "local-time"
               "cl-project"
               "do-urlencode")
  :components ((:module "v1/src"
                :components
                ((:module "core"
                  :components
                  ((:file "caveman" :depends-on ("route" "context" "app" "project"))
                   (:file "app" :depends-on ("request" "middleware/context"))
                   (:file "project" :depends-on ("context" "middleware/context"))
                   (:file "request")
                   (:file "response")
                   (:file "context" :depends-on ("request" "response"))
                   (:file "middleware/context" :depends-on ("context"))
                   (:file "skeleton")
                   (:file "route" :depends-on ("app"))
                   (:file "widget")))
                 (:module "lib"
                  :depends-on ("core")
                  :components
                  ((:module "widget"
                    :components
                    ((:file "form")))
                   (:module "view"
                    :serial t
                    :components
                    ((:file "function")
                     (:file "emb"))))))))
  :description "Web Application Framework for Common Lisp"
  :long-description #.(read-file-string (subpathname *load-pathname* "README.v1.markdown"))
  :in-order-to ((test-op (test-op "caveman-test")))
  :perform (load-op :after (o c)
             (warn "\"caveman\" is deprecated now. Did you mean \"caveman2\"?")))
