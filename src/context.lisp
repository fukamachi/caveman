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
                :make-response)
  (:export :request* :response*))

(cl-annot:enable-annot-syntax)

@export
(defclass <context> ()
     ((request :initarg :request :initform (make-request nil)
               :accessor request*)
      (response :initarg :response :initform (make-response 200 ())
                :accessor response*)))
