#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman
  (:use :cl)
  (:import-from :clack.response
                :redirect)
  (:import-from :caveman.route
                :url
                :url-for)
  (:import-from :caveman.app
                :next-route
                :lookup-route)
  (:import-from :caveman.context
                :*project*
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :with-context-variables)
  (:export :url
           :url-for
           :next-route
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :with-context-variables))

(cl-syntax:use-syntax :annot)

@export
(defun config (&optional key)
  (let ((conf (caveman.project:config *project*)))
    (if key (getf conf key) conf)))

@export
(defun current-mode ()
  (caveman.project:project-mode *project*))

@export
(defun app-path (&rest paths)
  (labels ((normalize-path (path)
             (etypecase path
               (keyword (config path))
               (pathname path))))
    (reduce
     (lambda (path1 path2)
       (merge-pathnames
        (normalize-path path2)
        (normalize-path path1)))
     paths
     :initial-value (config :application-root))))

@export
(defun redirect-to (url-or-action &rest params)
  (redirect (context :response)
            (etypecase url-or-action
              (string url-or-action)
              (symbol (apply #'url-for
                             url-or-action
                             params)))))

@export
(defun forward-to (symbol &rest params)
  (funcall (nth 2 (lookup-route *project* symbol)) params))

@export
(defun current-uri ()
  (caveman.request:request-uri *request*))

(doc:start)

@doc:NAME "
Caveman - main package.
"

@doc:DESCRIPTION "
This package is main package just for convenient. Your Caveman Application may use this package.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
