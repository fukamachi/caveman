#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

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
