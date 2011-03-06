(in-package :cl-user)
(defpackage ${application-name}-asd
  (:use :cl :asdf))
(in-package :${application-name}-asd)

(defsystem ${application-name}
  :version "0.1-SNAPSHOT"
  :depends-on (:caveman
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "${application-name}")
                 (:file "action")
                 (:file "view")
                 (:file "model")))))
