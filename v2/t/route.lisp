(in-package :cl-user)
(defpackage caveman2-test.route
  (:use :cl
        :caveman2
        :prove)
  (:import-from :lack.component
                :call))
(in-package :caveman2-test.route)

(plan 22)

(defvar *app*)

(setf *app* (make-instance '<app>))
(defroute "/" () "Welcome")
(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Welcome"))

(setf *app* (make-instance '<app>))
(defroute ("/") () "Welcome again")
(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Welcome again"))

(setf *app* (make-instance '<app>))
(defroute ("/" :method :post) () "Can you still get me?")
(is (first (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    404
    ":method :post")
(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :post)))
    '("Can you still get me?")
    ":method :post")

(setf *app* (make-instance '<app>))
(defroute (*app* "/") () "Hello")
(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello")
    "Specify an app")

(setf *app* (make-instance '<app>))
(defroute index "/" () "Hello")
(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello")
    "Named route")
(defroute index ("/new" :method :post) () "okay")
(is (third (call *app* '(:path-info "/new"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :post)))
    '("okay")
    "Named route")

(defroute hello ("/hello/([\\w]+)$" :regexp t) (&key captures)
  (format nil "Hello, ~A!" (first captures)))
(is (third (call *app* '(:path-info "/hello/Eitaro"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello, Eitaro!")
    "Regular expression")
(is (first (call *app* '(:path-info "/hello/Eitaro&Fukamachi"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    404
    "Regular expression")
(is (first (call *app* '(:path-info "/hello/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    404
    "Regular expression")

(setf *app* (make-instance '<app>))
(defroute index (*app* "/" :method :get) () "Hello")
(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello")
    "Full")
(defroute index (*app* "/" :method :get) () "Hello again")
(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello again")
    "Full")

(syntax:use-syntax :annot)

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

(is (third (call *app* '(:path-info "/"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Welcome")
    "@route")
(is (third (call *app* '(:path-info "/new"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Create something")
    "@route")
(is (third (call *app* '(:path-info "/new"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :post)))
    '("Create something")
    "@route")
(is (third (call *app* '(:path-info "/myname"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("I have no name yet.")
    "@route")
(is (third (call *app* '(:path-info "/myname"
                              :query-string "name=Eitaro"
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("My name is Eitaro.")
    "@route")
(is (third (call *app* '(:path-info "/hello"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello, Guest")
    "@route")
(is (third (call *app* '(:path-info "/hello/Eitaro"
                              :query-string ""
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello, Eitaro")
    "@route")
(is (third (call *app* '(:path-info "/hello/Eitaro"
                              :query-string "id=12345"
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :get)))
    '("Hello, Eitaro")
    "@route")

(defroute add-item (*app* "/post" :method :post) (&key _parsed)
  (with-output-to-string (s)
    (loop for item in (cdr (assoc "items" _parsed :test #'string=))
          do (format s "~&name:~S / price:~S~%"
                     (cdr (assoc "name" item :test #'string=))
                     (cdr (assoc "price" item :test #'string=))))))

(is (third (call *app* '(:path-info "/post"
                              :query-string "items[][name]=WiiU&items[][price]=30000&items[][name]=PS4&items[][price]=69000"
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :post)))
    '("name:\"WiiU\" / price:\"30000\"
name:\"PS4\" / price:\"69000\"
")
    "@route")

(is (third (call *app* '(:path-info "/post"
                              :query-string "_PARSED=&items[][name]=WiiU&items[][price]=30000&items[][name]=PS4&items[][price]=69000"
                              :raw-body (flex:make-in-memory-input-stream #())
                              :request-method :post)))
    '("name:\"WiiU\" / price:\"30000\"
name:\"PS4\" / price:\"69000\"
")
    "@route")

(finalize)
