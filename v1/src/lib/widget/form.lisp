#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman.widget.form
  (:use :cl
        :anaphora
        :caveman.widget))
(in-package :caveman.widget.form)

(cl-syntax:use-syntax :annot)

@export
(defclass <caveman-widget-form> (<caveman-widget>)
     ((action :initarg :action
              :type string
              :initform ""
              :accessor form-action)
      (method :initarg :method
              :type keyword
              :initform :post
              :accessor form-method)
      (components :initarg :components
                  :type list
                  :initform nil
                  :accessor form-components)))

(defmethod initialize-instance :after ((this <caveman-widget-form>) &rest initargs)
  @ignore initargs
  (sunless (view this)
    (setf it
          (lambda (&rest params)
            @ignore params
            (concatenate
             'string
             "<form method='" (princ-to-string (form-method this))
             "' action='" (form-action this) "'>"
             (apply #'concatenate 'string (mapcar #'render (form-components this)))
             "</form>")))))

@export
;; constructor
(defun make-form (&rest args)
  "A synonim for `(make-instance '<caveman-widget-form> ...)`."
  (apply #'make-instance '<caveman-widget-form> args))

@export
(defmethod add ((this <caveman-widget-form>) widget)
  "Add another widget to this form. That will be rendered in 'form' tag."
  (setf (form-components this)
        (append (list widget) (form-components this))))

@export
(defmethod validate ((this <caveman-widget-form>) values))

(doc:start)

@doc:NAME "
Caveman.Widget.Form - Widget for generating 'form'.
"

@doc:SYNOPSIS "
    (defvar form (make-form
                  :method :POST
                  :action \"/post\"
                  :components other-widgets))
    (add form widget)
    (render form)
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
