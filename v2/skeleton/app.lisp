(ql:quickload :<% @var name %>)

(defpackage <% @var name %>.app
  (:use :cl)
  (:import-from :clack
                :call)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>)
  (:import-from :clack.middleware.backtrace
                :<clack-middleware-backtrace>)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :<% @var name %>.web
                :*web*)
  (:import-from :<% @var name %>.config
                :config
                :productionp
                :*static-directory*))
(in-package :<% @var name %>.app)

(builder
 (<clack-middleware-static>
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (getf (config) :error-log)
     (make-instance '<clack-middleware-backtrace>
                    :output (getf (config) :error-log))
     nil)
 <clack-middleware-session>
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (call app env)))))
 *web*)
