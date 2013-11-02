(in-package :cl-user)
(defpackage <% @var name %>.web
  (:use :cl
        :caveman2
        :caveman2.db
        :<% @var name %>.config
        :<% @var name %>.view)
  (:export :*web*))
(in-package :<% @var name %>.web)

(defclass <web> (<app>) ())

(defparameter *web* (make-instance '<web>))

(defroute "/" ()
  (with-layout (:title "Welcome to Caveman2")
    (render #P"index.tmpl")))

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
