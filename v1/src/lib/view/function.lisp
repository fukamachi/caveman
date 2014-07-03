#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman.view.function
  (:use :cl))
(in-package :caveman.view.function)

(cl-syntax:use-syntax :annot)

@export
(defun render (fn params)
  "Render function for Functions."
  (apply fn params))
