(in-package :cl-user)
(defpackage <% @var name %>.view
  (:use :cl)
  (:import-from :<% @var name %>.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*)
  (:import-from :clack.response
                :headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :<% @var name %>.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (headers *response* :content-type) "application/json")
  (encode-json object))


;;
;; Execute package definition

(defpackage <% @var name %>.djula
  (:use :cl)
  (:import-from :<% @var name %>.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :<% @var name %>.djula))
