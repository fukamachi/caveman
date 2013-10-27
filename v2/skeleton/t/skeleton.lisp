(in-package :cl-user)
(defpackage <% @var name %>-test
  (:use :cl
        :<% @var name %>
        :cl-test-more))
(in-package :<% @var name %>-test)

(plan nil)

;; blah blah blah.

(finalize)
