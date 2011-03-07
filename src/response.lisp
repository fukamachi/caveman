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
(defclass <response> (clack.response:<response>) ())

@export
(defun make-response (&optional status headers body)
  (make-instance '<response>
     :status status
     :headers headers
     :body body))
