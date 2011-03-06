#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.view
  (:use :cl
        :clack)
  (:export :headers))

(cl-annot:enable-annot-syntax)

@export
(defvar *default-header* '(:content-type "text/html; charset=utf-8"))

@export
(defclass <view> (<component>)
     ((headers :initarg :headers
               :initform *default-header*
               :accessor headers)))

(defmethod call ((this <view>) params)
  `(200 ,(headers this) ,(render this params)))

@export
(defgeneric render (view params))
