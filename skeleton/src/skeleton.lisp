#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(clack.util:namespace <% @var name %>
  (:use :cl
        :clack)
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
                :*app*))

(cl-syntax:use-syntax :annot)

@export
(defclass <<% @var name %>> (<project>) ())

@export
(defvar *project* nil)

(defmethod build ((this <<% @var name %>>) &optional app)
  @ignore app
  (call-next-method
   this
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

@export
(defun restart (&key (mode :dev) (debug t) lazy)
  (when *project*
    (setf mode (project-mode *project*))
    (setf debug (debug-mode-p *project*))
    (setf lazy *builder-lazy-p*))
  (stop)
  (start :mode mode :debug debug :lazy lazy))
