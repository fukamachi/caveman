#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(clack.util:namespace <% @var name %>.controller
  (:use :cl
        :caveman
        :<% @var name %>.app)
  (:import-from :<% @var name %>.view.emb
                :render))

(cl-syntax:use-syntax :annot)

@url GET "/"
(defun index (params)
  @ignore params
  (render "index.html"))

@url POST "/"
(defun index-post (params)
  @ignore params
  "Hello, Caveman!")
