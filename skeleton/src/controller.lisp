(clack.util:namespace <% @var name %>.controller
  (:use :cl
        :caveman
        :<% @var name %>.app)
  (:import-from :<% @var name %>.view.emb
                :render))

(cl-annot:enable-annot-syntax)

@url GET "/"
(defun index (params)
  (render
   "index.tmpl"
   params))

@url POST "/"
(defun index-post (params)
  @ignore params
  "Hello, Caveman!")
