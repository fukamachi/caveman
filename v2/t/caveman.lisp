#|
  This file is a part of caveman project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2-test
  (:use :cl
        :caveman2
        :cl-test-more))
(in-package :caveman2-test)

(plan nil)

(defvar *app*)

(setf *app* (make-instance '<app>))
(defroute "/" () "Welcome")
(is (third (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    '("Welcome"))

(setf *app* (make-instance '<app>))
(defroute ("/") () "Welcome again")
(is (third (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    '("Welcome again"))

(setf *app* (make-instance '<app>))
(defroute ("/" :method :post) () "Can you still get me?")
(is (first (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    404
    ":method :post")
(is (third (clack:call *app* '(:path-info "/"
                               :request-method :post)))
    '("Can you still get me?")
    ":method :post")

(setf *app* (make-instance '<app>))
(defroute (*app* "/") () "Hello")
(is (third (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    '("Hello")
    "Specify an app")

(setf *app* (make-instance '<app>))
(defroute index "/" () "Hello")
(is (third (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    '("Hello")
    "Named route")
(defroute index ("/new" :method :post) () "okay")
(is (third (clack:call *app* '(:path-info "/new"
                               :request-method :post)))
    '("okay")
    "Named route")

(setf *app* (make-instance '<app>))
(defroute index (*app* "/" :method :get) () "Hello")
(is (third (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    '("Hello")
    "Full")
(defroute index (*app* "/" :method :get) () "Hello again")
(is (third (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    '("Hello again")
    "Full")

(finalize)
