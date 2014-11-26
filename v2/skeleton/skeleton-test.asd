(in-package :cl-user)
(defpackage <% @var name %>-test-asd
  (:use :cl :asdf))
(in-package :<% @var name %>-test-asd)

(defsystem <% @var name %>-test
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on (:<% @var name %>
               :prove)
  :components ((:module "t"
                :components
                ((:file "<% @var name %>"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
