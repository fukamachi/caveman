(in-package :cl-user)
(defpackage <% @var name %>-test
  (:use :cl
        :<% @var name %>
        :rove))
(in-package :<% @var name %>-test)

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
