#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.middleware.context
  (:use :cl
        :clack)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax)
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

(use-syntax annot-syntax)

@export
(defclass <caveman-middleware-context> (<middleware>) ()
  (:documentation "Clack Middleware to set context for each request."))

(defmethod call ((this <caveman-middleware-context>) req)
  (let* ((*context* (make-context req))
         (*request* (context :request))
         (*response* (context :response))
         (*session* (context :session))
         (result (call-next this req)))
    (setf (body *response*) result)
    (finalize *response*)))

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
