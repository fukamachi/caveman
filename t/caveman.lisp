(clack.util:namespace caveman-test
  (:use :cl
        :caveman
        :cl-test-more
        :drakma
        :myapp))

(plan 2)

(cl-annot:enable-annot-syntax)

(is (http-request "http://localhost:8080/")
    "Hello, Caveman!"
    "index")

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
                  :parameters '(("name" . "fukamachi")
                                ("pass" . "lispiscool")))
    "Name: fukamachi / Pass: lispiscool"
    "POST")

(finalize)
