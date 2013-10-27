#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2.exception
  (:use :cl)
  (:import-from :clack.http-status
                :http-status-reason)
  (:export :caveman-exception
           :throw-code
           :exception-code))
(in-package :caveman2.exception)

(define-condition caveman-exception (simple-error)
  ((code :initarg :code :type integer :initform 500
         :reader exception-code))
  (:documentation "")
  (:report
   (lambda (condition stream)
     (let ((code (exception-code condition)))
       (format stream
               "~D ~A"
               code
               (http-status-reason code))))))

(defun throw-code (code)
  ""
  (error 'caveman-exception :code code))
