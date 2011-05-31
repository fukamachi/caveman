(clack.util:namespace caveman-test
  (:use :cl
        :caveman
        :cl-test-more
        :drakma
        :myapp))

(plan 8)

(cl-annot:enable-annot-syntax)

(is (http-request "http://localhost:8080/")
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"ja\" xml:lang=\"ja\">
<head>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
  <title>Welcome to Caveman!</title>
  <link rel=\"stylesheet\" href=\"./public/main.css\" type=\"text/css\" media=\"screen, tv, print\" charset=\"UTF-8\" />
</head>
<body>
<h1>Hello, Caveman!</h1>
</body>
</html>
"
    "index (GET)")
(is (http-request "http://localhost:8080/" :method :POST)
    "Hello, Caveman!"
    "index (POST)")

(is (http-request "http://localhost:8080/not-found-hoge")
    nil
    "not found")

@url GET "/member/:id/profile"
(defun member-profile (params)
  (format nil "Member Profile: ~d" (getf params :id)))

(is (http-request "http://localhost:8080/member/11/profile")
    "Member Profile: 11"
    "GET")

@url POST "/login"
(defun do-login (params)
  (format nil "Name: ~A / Pass: ~A"
          (getf params :name)
          (getf params :pass)))

(is (http-request "http://localhost:8080/login"
                  :method :post
                  :parameters '(("NAME" . "fukamachi")
                                ("PASS" . "lispiscool")))
    "Name: fukamachi / Pass: lispiscool"
    "POST")

@url GET "/context"
(defun ctx-test (params)
  @ignore params
  (princ-to-string (type-of *context*)))

(is (http-request "http://localhost:8080/context")
    "HASH-TABLE"
    "context")

@url GET "/request"
(defun req-test (params)
  @ignore params
  (princ-to-string (type-of *request*)))

(is (http-request "http://localhost:8080/request")
    "<REQUEST>"
    "request")

@url GET "/response"
(defun res-test (params)
  @ignore params
  (princ-to-string (type-of *response*)))

(is (http-request "http://localhost:8080/response")
    "<RESPONSE>"
    "response")

(finalize)
