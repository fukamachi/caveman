#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2.app
  (:use :cl)
  (:import-from :caveman2.exception
                :caveman-exception
                :exception-code
                :throw-code)
  (:import-from :clack
                :call)
  (:import-from :clack.response
                :status
                :headers)
  (:import-from :ningle
                :next-route
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :make-request
                :make-response)
  (:import-from :ningle.app
                :not-found)
  (:export :<app>
           :next-route
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :make-request
           :make-response
           :on-exception))
(in-package :caveman2.app)

(defclass <app> (ningle:<app>) ())

(defmethod call ((this <app>) env)
  (declare (ignore env))
  (handler-case (call-next-method)
    (caveman-exception (c)
      (let ((code (exception-code c)))
        (setf (status *response*) code)
        (or (on-exception this code)
            (princ-to-string c))))))

(defmethod not-found ((this <app>))
  (throw-code 404))

(defmethod make-response ((app <app>) &optional status headers body)
  (declare (ignore status headers body))
  (let ((res (call-next-method)))
    (unless (headers res :X-Content-Type-Options)
      (setf (headers res :X-Content-Type-Options) "nosniff"))
    (unless (headers res :X-Frame-Options)
      (setf (headers res :X-Frame-Options) "DENY"))
    (unless (headers res :Cache-Control)
      (setf (headers res :Cache-Control) "private"))
    res))

(defmethod on-exception ((this <app>) code)
  nil)
