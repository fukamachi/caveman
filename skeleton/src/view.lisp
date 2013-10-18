(in-package :cl-user)
(defpackage <% @var name %>.view
  (:use :cl)
  (:import-from :<% @var name %>.config
                :*template-directory*)
  (:import-from :cl-emb
                :execute-emb)
  (:export :*default-layout-path*
           :render
           :with-layout))
(in-package :<% @var name %>.view)

(defvar *default-layout-directory* #P"layouts/")
(defvar *default-layout-path* #P"default.tmpl")

(defun render (template-path &optional env)
  (emb:execute-emb
   (merge-pathnames template-path
                    *template-directory*)
   :env env))

(defmacro with-layout ((&rest env-for-layout) &body body)
  (let ((layout-path (merge-pathnames *default-layout-path*
                                      *default-layout-directory*)))
    (when (pathnamep (car env-for-layout))
      (setf layout-path (pop env-for-layout)))

    `(emb:execute-emb
      (merge-pathnames ,layout-path
                       *template-directory*)
      :env (list :content (progn ,@body)
                 ,@env-for-layout))))
