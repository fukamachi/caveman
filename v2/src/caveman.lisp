#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2
  (:use :cl)
  (:import-from :caveman2.route
                :defroute
                :route)
  (:import-from :caveman2.skeleton
                :make-project)
  (:import-from :caveman2.app
                :<app>
                :next-route
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :make-request
                :make-response
                :on-exception)
  (:import-from :caveman2.helper
                :redirect
                :url-for)
  (:import-from :caveman2.exception
                :throw-code)
  (:import-from :clack.request
                :env
                :request-method
                :script-name
                :path-info
                :server-name
                :server-port
                :server-protocol
                :request-uri
                :uri-scheme
                :remote-addr
                :remote-port
                :query-string
                :raw-body
                :content-length
                :content-type
                :clack-handler
                :referer
                :user-agent
                :securep
                :cookies
                :body-parameter
                :query-parameter
                :parameter)
  (:import-from :clack.response
                :status
                :headers
                :push-header
                :body
                :set-cookies)
  (:export :defroute
           :route
           :<app>
           :next-route
           :redirect
           :url-for
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :make-request
           :make-response
           :make-project
           :on-exception
           :throw-code

           ;; from Clack.Request
           :request-method
           :script-name
           :path-info
           :server-name
           :server-port
           :server-protocol
           :request-uri
           :uri-scheme
           :remote-addr
           :remote-port
           :query-string
           :raw-body
           :content-length
           :content-type
           :clack-handler
           :referer
           :user-agent
           :securep
           :cookies
           :body-parameter
           :query-parameter
           :parameter

           ;; from Clack.Response
           :status
           :headers
           :push-header
           :body
           :set-cookies))
(in-package :caveman2)
