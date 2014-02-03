#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman.middleware.context
  (:use :cl
        :clack)
  (:import-from :caveman.context
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :make-context)
  (:import-from :caveman.response
                :make-response
                :body
                :finalize))
(in-package :caveman.middleware.context)

(cl-syntax:use-syntax :annot)

@export
(defclass <caveman-middleware-context> (<middleware>) ()
  (:documentation "Clack Middleware to set context for each request."))

(defmethod call ((this <caveman-middleware-context>) req)
  (let* ((*context* (make-context req))
         (*request* (context :request))
         (*response* (context :response))
         (*session* (context :session))
         (result (call-next this req)))
    (if (and result (listp result))
        result
        (progn (setf (body *response*) result)
               (finalize *response*)))))

(doc:start)

@doc:NAME "
Caveman.Middleware.Context - Clack Middleware to set context for each request.
"

@doc:DESCRIPTION "
This is a Clack Middleware to ensure context is set for each request.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Caveman.Context
"
