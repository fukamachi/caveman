(in-package :cl-user)
(defpackage caveman2.app
  (:use :cl)
  (:import-from :caveman2.exception
                :caveman-exception
                :exception-code
                :throw-code
                :caveman-redirection
                :redirection-to
                :redirection-code)
  (:import-from :lack.component
                :call)
  (:import-from :lack.response
                :response-status
                :response-headers)
  (:import-from :ningle
                :next-route
                :clear-routing-rules
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
           :clear-routing-rules
           :*current-app*
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :make-request
           :make-response
           :on-exception
           :find-package-app))
(in-package :caveman2.app)

(defparameter *current-app* nil)

(defclass <app> (ningle:<app>) ())

(defvar *package-app-map* (make-hash-table :test 'eq))

(defmethod initialize-instance :after ((app <app>) &key)
  (setf (gethash *package* *package-app-map*) app))

(defun find-package-app (package)
  (gethash package *package-app-map*))

(defmethod call ((this <app>) env)
  (declare (ignore env))
  (let ((*current-app* this))
    (handler-case (call-next-method)
      (caveman-exception (c)
        (let ((code (exception-code c)))
          (setf (response-status *response*) code)
          (or (on-exception this code)
              (princ-to-string c))))
      (caveman-redirection (c)
        (let ((to (redirection-to c))
              (code (redirection-code c)))
          (setf (getf (response-headers *response*) :location) to)
          (setf (response-status *response*) code)
          to)))))

(defmethod not-found ((this <app>))
  (throw-code 404))

(defmethod make-response ((app <app>) &optional status headers body)
  (declare (ignore status headers body))
  (let ((res (call-next-method)))
    (unless (getf (response-headers res) :content-type)
      (setf (getf (response-headers res) :content-type) "text/html"))
    (unless (getf (response-headers res) :X-Content-Type-Options)
      (setf (getf (response-headers res) :X-Content-Type-Options) "nosniff"))
    (unless (getf (response-headers res) :X-Frame-Options)
      (setf (getf (response-headers res) :X-Frame-Options) "DENY"))
    (unless (getf (response-headers res) :Cache-Control)
      (setf (getf (response-headers res) :Cache-Control) "private"))
    res))

(defmethod on-exception ((this <app>) code)
  nil)
