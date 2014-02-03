(in-package :cl-user)
(defpackage caveman-test
  (:use :cl
        :caveman
        :cl-test-more
        :drakma
        :myapp.app)
  (:import-from :caveman-test.init
                :*myapp-url*))
(in-package :caveman-test)

(plan 10)

(cl-syntax:use-syntax :annot)

(is (http-request *myapp-url*)
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"ja\" xml:lang=\"ja\">
<head>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
  <title>Welcome to Caveman!</title>
  <link rel=\"stylesheet\" href=\"/static/css/main.css\" type=\"text/css\" media=\"screen, tv, print\" charset=\"UTF-8\" />
</head>
<body>
<h1>Hello, Caveman!</h1>
</body>
</html>
"
    "index (GET)")
(is (http-request *myapp-url* :method :POST)
    "Hello, Caveman!"
    "index (POST)")

(multiple-value-bind (body status)
    (http-request *myapp-url*
                  :method :HEAD)
  @ignore body
  (is status 200 "index (HEAD)"))

(is (http-request (format nil "~A/not-found-hoge" *myapp-url*))
    nil
    "not found")

@url GET "/member/:id/profile"
(defun member-profile (params)
  (format nil "Member Profile: ~d" (getf params :id)))

(is (http-request (format nil "~A/member/11/profile" *myapp-url*))
    "Member Profile: 11"
    "GET")

@url POST "/login"
(defun do-login (params)
  (format nil "Name: ~A / Pass: ~A"
          (getf params :name)
          (getf params :pass)))

(is (http-request (format nil "~A/login" *myapp-url*)
                  :method :post
                  :parameters '(("NAME" . "fukamachi")
                                ("PASS" . "lispiscool")))
    "Name: fukamachi / Pass: lispiscool"
    "POST")

@url GET "/context"
(defun ctx-test (params)
  @ignore params
  (princ-to-string (type-of *context*)))

(is (http-request (format nil "~A/context" *myapp-url*))
    "HASH-TABLE"
    "context")

@url GET "/request"
(defun req-test (params)
  @ignore params
  (princ-to-string (type-of *request*)))

(is (http-request (format nil "~A/request" *myapp-url*))
    "<REQUEST>"
    "request")

@url GET "/response"
(defun res-test (params)
  @ignore params
  (princ-to-string (type-of *response*)))

(is (http-request (format nil "~A/response" *myapp-url*))
    "<RESPONSE>"
    "response")

@url GET "/next"
(defun next-test (params)
  @ignore params
  (format nil "(~A)" (next-route)))

@url GET "/next"
(defun next-test2 (params)
  @ignore params
  "This is the next route.")

(is (http-request (format nil "~A/next" *myapp-url*))
    "(This is the next route.)"
    "next-route")

(finalize)
