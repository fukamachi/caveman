#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(in-package :cl-user)
(defpackage <% @var name %>.view.emb
  (:use :cl)
  (:import-from :caveman
                :config))
(in-package :<% @var name %>.view.emb)

(cl-syntax:use-syntax :annot)

@export
(defun render (file &optional params)
  (caveman.view.emb:render
   (merge-pathnames file
    (merge-pathnames
     (config :template-path)
     (config :application-root)))
   params))
