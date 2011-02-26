#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.layout
  (:use :cl)
  (:import-from :caveman
                :*application-name*)
  (:import-from :caveman.view
                :<view>
                :render
                :headers)
  (:import-from :cl-markup
                :markup)
  (:export :javascripts
           :stylesheets
           :title))

(cl-annot:enable-annot-syntax)

@export
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

@export
(defmethod render :around ((this <default-layout>) params)
  (markup
   (:html
    (:head
     (:meta
      :http-equiv "Content-Type"
      :content (getf (headers this) :content-type))
     (mapcar #'(lambda (style)
                 (markup
                  (:link :rel "stylesheet" :href style :type "text/css")))
      (stylesheets this))
     (mapcar #'(lambda (js)
                 (markup
                  (:script :src js :type "text/javascript" nil)))
      (javascripts this))
     (:title (title this)))
    (:body (call-next-method)))))
