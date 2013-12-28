(in-package :cl-user)
(defpackage <% @var name %>
  (:use :cl)
  (:import-from :<% @var name %>.config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop))
(in-package :<% @var name %>)

(defvar *appfile-path*
  (asdf:system-relative-pathname :<% @var name %> #P"app.lisp"))

(let (handler)
  (defun start (&rest args &key server port debug &allow-other-keys)
    (when handler
      (error "Server is already started."))
    (setf handler
          (apply #'clackup *appfile-path* args)))

  (defun stop ()
    (prog1
     (clack:stop handler)
     (setf handler nil))))
