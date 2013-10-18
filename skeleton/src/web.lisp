(in-package :cl-user)
(defpackage <% @var name %>.web
  (:use :cl)
  (:import-from :<% @var name %>.config
                :*template-directory*)
  (:import-from :<% @var name %>.view
                :render
                :with-layout)
  (:import-from :caveman
                :<app>
                :defroute
                :next-route
                :on-exception)
  (:export :*web*))
(in-package :<% @var name %>.web)

(defclass <web> (<app>) ())

(defparameter *web* (make-instance '<web>))

(defroute (*web* "/") ()
  (with-layout (:title "Welcome to Caveman")
    (render #P"index.tmpl")))

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
