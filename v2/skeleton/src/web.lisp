(in-package :cl-user)
(defpackage <% @var name %>.web
  (:use :cl
        :caveman2
        :caveman2.db
        :<% @var name %>.config
        :<% @var name %>.view)
  (:export :*web*))
(in-package :<% @var name %>.web)

;;
;; Application

(defclass <web> (<app>) ())
(defparameter *web* (make-instance '<web>))

;;
;; Routing rules

(defroute "/" ()
  (with-layout (:title "Welcome to Caveman2")
    (render #P"index.tmpl")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
