#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman.widget
  (:use :cl
        :clack)
  (:export :view))
(in-package :caveman.widget)

(cl-syntax:use-syntax :annot)

@export
(defclass <caveman-widget> ()
     ((view :initarg :view
            :type (or null function <component>)
            :initform nil
            :accessor view))
  (:documentation "Base class of view widget."))

@export
(defmethod render ((this <caveman-widget>) &optional params)
  (if (view this)
      (call (view this) params)
      ""))

(doc:start)

@doc:NAME "
Caveman.Widget - Base component for View widget.
"

@doc:DESCRIPTION "
* <caveman-widget>
* render
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"
