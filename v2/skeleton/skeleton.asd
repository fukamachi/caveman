(in-package :cl-user)
(defpackage <% @var name %>-asd
  (:use :cl :asdf))
(in-package :<% @var name %>-asd)

(defsystem <% @var name %>
  :version "0.1"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre

               ;; HTML Template
               :cl-emb

               ;; for rendering JSON
               :yason
               :trivial-types

               ;; for CL-DBI
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "<% @var description %>"
  :in-order-to ((test-op (load-op <% @var name %>-test))))
