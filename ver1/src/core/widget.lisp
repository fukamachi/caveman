#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.widget
  (:use :cl
        :clack)
  (:export :view))

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
* Eitarow Fukamachi (e.arrows@gmail.com)
"
