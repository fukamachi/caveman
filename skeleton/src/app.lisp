#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#

(clack.util:namespace <% @var name %>.app
  (:use :cl)
  (:import-from :caveman.app
                :<app>))

(cl-annot:enable-annot-syntax)

@export
(defclass <<% @var name %>-app> (<app>) ())

@export
(defvar *app* (make-instance '<<% @var name %>-app>))
