#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.view
  (:use :cl
        :clack))

(cl-annot:enable-annot-syntax)

@export
(defvar *default-content-type "text/html")

@export
(defclass <view> (<component>)
     ((headers :initarg :headers
               :initform `(:content-type ,*default-content-type)
               :accessor headers)))

@export
(defmethod call ((this <view>) arg)
  @ignore arg
  `(200 ,(headers this) ,(render this)))

@export
(defgeneric render (view))
