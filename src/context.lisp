#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.context
  (:use :cl)
  (:import-from :caveman.request
                :make-request)
  (:import-from :caveman.response
                :make-response))

(cl-annot:enable-annot-syntax)

@export
(defvar *context* nil)

@export
(defvar *request* nil)

@export
(defvar *response* nil)

@export
(defun make-context (req)
  (let ((*context* (make-hash-table)))
    (setf (context :request) (make-request req))
    (setf (context :response) (make-response 200 ()))
    *context*))

@export
(defun context (&optional key)
  (if key (gethash key *context*) *context*))

@export
(defun (setf context) (val key)
  (setf (gethash key *context*) val))
