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
(defsystem "<% @var name %>"
  :version "0.1"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on ("clack"
               "caveman"
               "cl-syntax"
               "cl-syntax-annot"
               "cl-ppcre")
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
                 (:file "controller" :depends-on ("app")))))
  :description "<% @var description %>"
  ;; :long-description #.(read-file-string (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (load-op "<% @var name %>-test"))))
