(in-package :cl-user)
(defpackage <% @var name %>.view
  (:use :cl)
  (:import-from :<% @var name %>.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*)
  (:import-from :clack.response
                :headers)
  (:import-from :cl-emb
                :*escape-type*
                :*case-sensitivity*
                :execute-emb)
  (:import-from :yason
                :encode
                :encode-plist
                :encode-alist)
  (:import-from :trivial-types
                :property-list-p
                :association-list-p)
  (:export :*default-layout-path*
           :render
           :render-json
           :with-layout))
(in-package :<% @var name %>.view)

(defvar *default-layout-directory* #P"layouts/")
(defvar *default-layout-path* #P"default.tmpl")

(defun render (template-path &optional env)
  (let ((emb:*escape-type* :html)
        (emb:*case-sensitivity* nil))
    (emb:execute-emb
     (merge-pathnames template-path
                      *template-directory*)
     :env env)))

(defun render-json (object)
  (setf (headers *response* :content-type) "application/json")
  (with-output-to-string (s)
    (cond
      ((property-list-p object) (encode-plist object s))
      ((association-list-p object) (encode-alist object s))
      (T (encode object s)))))

(defmacro with-layout ((&rest env-for-layout) &body body)
  (let ((layout-path (merge-pathnames *default-layout-path*
                                      *default-layout-directory*)))
    (when (pathnamep (car env-for-layout))
      (setf layout-path (pop env-for-layout)))

    `(let ((emb:*escape-type* :html)
           (emb:*case-sensitivity* nil))
       (emb:execute-emb
        (merge-pathnames ,layout-path
                         *template-directory*)
        :env (list :content (progn ,@body)
                   ,@env-for-layout)))))

;; Define functions that are available in templates.
(defpackage cl-emb-intern
  (:import-from :<% @var name %>.config
                :config)
  (:import-from :caveman2
                :url-for))
