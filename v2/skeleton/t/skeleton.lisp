(in-package :cl-user)
(defpackage <% @var name %>-test
  (:use :cl
        :<% @var name %>
        :prove))
(in-package :<% @var name %>-test)

(plan nil)

;; blah blah blah.

(finalize)
