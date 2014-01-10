(in-package :cl-user)
(defpackage caveman2-test
  (:use :cl
        :caveman2
        :cl-test-more))
(in-package :caveman2-test)

(plan 18)

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

(cl-annot:enable-annot-syntax)

(setf *app* (make-instance '<app>))

@route GET "/"
(defun index ()
  "Welcome")
@route (GET POST) "/new"
(defun new ()
  "Create something")
@route GET "/myname"
(lambda (&key |name|)
  (if |name|
      (format nil "My name is ~A." |name|)
      "I have no name yet."))
@route GET "/hello"
@route GET "/hello/:name"
(defun say-hello (&key (name "Guest"))
  (format nil "Hello, ~A" name))

@route POST "/post"
(defun add-item (&key |items|)
  (format nil "~S" |items|))

(is (third (clack:call *app* '(:path-info "/"
                               :request-method :get)))
    '("Welcome")
    "@route")
(is (third (clack:call *app* '(:path-info "/new"
                               :request-method :get)))
    '("Create something")
    "@route")
(is (third (clack:call *app* '(:path-info "/new"
                               :request-method :post)))
    '("Create something")
    "@route")
(is (third (clack:call *app* '(:path-info "/myname"
                               :request-method :get)))
    '("I have no name yet.")
    "@route")
(is (third (clack:call *app* '(:path-info "/myname"
                               :query-string "name=Eitarow"
                               :request-method :get)))
    '("My name is Eitarow.")
    "@route")
(is (third (clack:call *app* '(:path-info "/hello"
                               :request-method :get)))
    '("Hello, Guest")
    "@route")
(is (third (clack:call *app* '(:path-info "/hello/Eitarow"
                               :request-method :get)))
    '("Hello, Eitarow")
    "@route")
(is (third (clack:call *app* '(:path-info "/hello/Eitarow"
                               :query-string "id=12345"
                               :request-method :get)))
    '("Hello, Eitarow")
    "@route")

(is (third (clack:call *app* '(:path-info "/post"
                               :query-string "items[][name]=WiiU&items[][price]=30000&items[][name]=PS4&items[][price]=69000"
                               :request-method :post)))
    '("((:|name| \"WiiU\" :|price| \"30000\") (:|name| \"PS4\" :|price| \"69000\"))")
    "@route")

(finalize)
