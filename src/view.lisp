#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.view
  (:use :cl
        :clack)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax)
  (:export :headers))

(use-syntax annot-syntax)

@export
(defvar *default-header* '(:content-type "text/html; charset=utf-8")
  "Default HTTP header for `<view>'.")

@export
(defclass <view> (<component>)
     ((headers :initarg :headers
               :initform *default-header*
               :accessor headers))
  (:documentation "Class for Caveman View."))

(defmethod call ((this <view>) params)
  "Returns a Clack response."
  `(200 ,(headers this) ,(render this params)))

@export
(defgeneric render (view params)
  (:documentation "Returns a response as a string, especially a HTML source."))

@export
(defmethod render ((view function) params)
  "Render method for Functions."
  (apply view params))

(doc:start)

@doc:NAME "
Caveman.View - Template system for Caveman.
"

@doc:SYNOPSIS "
    (defclass <default-layout> (<view>)
         ((javascripts :initarg :javascripts
                       :initform nil
                       :accessor javascripts)
          (stylesheets :initarg :stylesheets
                       :initform nil
                       :accessor stylesheets)
          (title :initarg :title
                 :initform *application-name*
                 :accessor title)))
    
    (defmethod render :around ((this <default-layout>) params)
      (markup
       (:html
        (:head
         (:meta
          :http-equiv \"Content-Type\"
          :content (getf (headers this) :content-type))
         (mapcar #'(lambda (style)
                     (markup
                      (:link :rel \"stylesheet\" :href style :type \"text/css\")))
          (stylesheets this))
         (mapcar #'(lambda (js)
                     (markup
                      (:script :src js :type \"text/javascript\" nil)))
          (javascripts this))
         (:title (title this)))
        (:body (call-next-method)))))
"

@doc:DESCRIPTION "
Caveman.View provides a way to manage View layer. This is a kind of \"Template System\" in other languages world.

The advantage of `<view>' is possible to separate each parts in a page into methods. You can add a method `header' and `footer' for it. Noteworthly, you can also override them. This is well known \"Template Inheritance\" in other world.

But, of course, this is an optional in Caveman. You can use a function as a view for instead.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
