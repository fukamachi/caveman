#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(in-package :cl-user)
(defpackage <% @var name %>.app
  (:use :cl)
  (:import-from :caveman.app
                :<app>))
(in-package :<% @var name %>.app)

(cl-syntax:use-syntax :annot)

@export
(defclass <<% @var name %>-app> (<app>) ())

@export
(defvar *app* (make-instance '<<% @var name %>-app>))
