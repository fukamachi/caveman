#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.view
  (:use :cl
        :clack)
  (:import-from :cl-markup
                :markup)
  (:export :headers))

(cl-annot:enable-annot-syntax)

@export
(defvar *default-content-type* "text/html; charset=utf-8")

@export
(defclass <view> (<component>)
     ((headers :initarg :headers
               :initform `(:content-type ,*default-content-type*)
               :accessor headers)))

@export
(defmethod call ((this <view>) params)
  `(200 ,(headers this) ,(render this params)))

@export
(defmethod call ((this function) params)
  `(200 (:content-type ,*default-content-type*) ,(funcall this params)))

@export
(defgeneric render (view params))
