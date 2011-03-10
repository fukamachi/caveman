#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.response
  (:use :cl
        :clack.response)
  (:shadow :<response>)
  (:export :status
           :headers
           :push-header
           :body
           :set-cookie
           :redirect
           :finalize))

(cl-annot:enable-annot-syntax)

@export
(defclass <response> (clack.response:<response>) ()
  (:documentation "Class for Caveman Response."))

@export
(defun make-response (&optional status headers body)
  "Construct a response instance."
  (make-instance '<response>
     :status status
     :headers headers
     :body body))

(doc:start)

@doc:NAME "
Caveman.Response - Response class for Caveman.
"

@doc:DESCRIPTION "
Caveman.Response is a response class for Caveman. Caveman creates a `<response>' instance for each request and bind it to `*response*'. It will be used in response phase.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Response
"
