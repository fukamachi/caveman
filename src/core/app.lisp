#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.app
  (:use :cl
        :clack
        :caveman.middleware.context)
  (:import-from :clack.util.route
                :match)
  (:import-from :caveman.context
                :*request*
                :*response*)
  (:import-from :caveman.request
                :request-method
                :path-info
                :parameter))

(cl-syntax:use-syntax :annot)

@export
(defclass <app> (<component>)
     ((routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules))
  (:documentation "Base class for Caveman Application. All Caveman Application must inherit this class."))

(defmethod call :around ((this <app>) env)
  (call (wrap
         (make-instance '<caveman-middleware-context>)
         (lambda (env)
           (call-next-method this env)))
        env))

(defmethod call ((this <app>) env)
  "Overriding method. This method will be called for each request."
  @ignore env
  (let* ((req *request*)
         (path-info (path-info req))
         (method (request-method req)))
    (loop for (nil rule fn) in (reverse (routing-rules this))
          do (multiple-value-bind (matchp params)
                 (match rule method path-info)
               (when matchp
                 (setf (slot-value req 'clack.request::query-parameters)
                       (append
                        params
                        (slot-value req 'clack.request::query-parameters)))
                 (let ((res (call fn (parameter req))))
                   (unless (eq res (next-route))
                     (return res)))))
          finally
          (progn (setf (clack.response:status *response*) 404)
                 nil))))

@export
(defmethod add-route ((this <app>) routing-rule)
  "Add a routing rule to the Application."
  (setf (routing-rules this)
        (delete (car routing-rule)
                (routing-rules this)
                :key #'car))
  (push routing-rule
        (routing-rules this)))

(defparameter +next-route+ '#:next-route)

@export
(defun next-route ()
  +next-route+)

@export
(defmethod lookup-route ((this <app>) symbol)
  "Lookup a routing rule with SYMBOL from the application."
  (loop for rule in (reverse (routing-rules this))
        if (eq (first rule) symbol) do
          (return rule)))

(doc:start)

@doc:NAME "
Caveman.App - Caveman Application Class.
"

@doc:SYNOPSIS "
    (defclass <myapp-app> (<app>) ())
    (defvar *app* (make-instance '<myapp-app>))
    (call *app*)
"

@doc:DESCRIPTION "
Caveman.App provides a base class `<app>' for Caveman Applications.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
"
