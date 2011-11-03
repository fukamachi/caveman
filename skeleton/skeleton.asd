#|
  This file is a part of <% @var name %> project.
<% @if author %>  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %>|#
<%
(when (or (getf env :description)
          (getf env :author))
%>
#|
<% @if description %>  <% @var description %>

<% @endif %><% @if author %>  Author: <% @var author %><% @if email %> (<% @var email %>)<% @endif %><% @endif %>
|#
<% ) %>
(in-package :cl-user)
(defpackage <% @var name %>-asd
  (:use :cl :asdf))
(in-package :<% @var name %>-asd)

(defsystem <% @var name %>
  :version "0.1-SNAPSHOT"
  :depends-on (:clack
               :clack-middleware-clsql
               :caveman
               :cl-annot)
  :components ((:module "lib"
                :components
                ((:module "view"
                  :components
                  ((:file "emb")))))
               (:module "src"
                :depends-on ("lib")
                :components
                ((:file "app")
                 (:file "<% @var name %>" :depends-on ("app"))
                 (:file "controller" :depends-on ("app"))))))
