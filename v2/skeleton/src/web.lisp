(in-package :cl-user)
(defpackage <% @var name %>.web
  (:use :cl
        :caveman2
        :<% @var name %>.config
        :<% @var name %>.view
        :<% @var name %>.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :<% @var name %>.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render (merge-pathnames #P"_errors/404.html"
                           *template-directory*)))
