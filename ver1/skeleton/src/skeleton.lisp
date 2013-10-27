#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(in-package :cl-user)
(defpackage <% @var name %>
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static
        :clack.middleware.session)
  (:shadow :stop)
  (:import-from :caveman
                :config)
  (:import-from :caveman.project
                :<project>
                :build
                :project-mode
                :debug-mode-p)
  (:import-from :clack.builder
                :*builder-lazy-p*)
  (:import-from :<% @var name %>.app
                :*app*)
  (:import-from :cl-ppcre
                :scan
                :regex-replace))
(in-package :<% @var name %>)

(cl-syntax:use-syntax :annot)

@export
(defclass <<% @var name %>> (<project>) ())

@export
(defvar *project* nil)

(defmethod build ((this <<% @var name %>>))
  (builder
   (<clack-middleware-static>
    :path (lambda (path)
            (when (ppcre:scan "^(?:/static/|/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
              (ppcre:regex-replace "^/static" path "")))
    :root (merge-pathnames (config :static-path)
                           (config :application-root)))
   <clack-middleware-session>
   <% @var name %>.app:*app*))

@export
(defun start (&key (mode :dev) (debug t) lazy port)
  (setf *project* (make-instance '<<% @var name %>>))
  (caveman.project:start *project* :mode mode :debug debug :lazy lazy :port port))

@export
(defun stop ()
  (when *project*
    (caveman.project:stop *project*)
    (setf *project* nil)))
