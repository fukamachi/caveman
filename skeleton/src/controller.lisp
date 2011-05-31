(clack.util:namespace ${application-name}.controller
  (:use :cl
        :caveman
        :${application-name}))

(cl-annot:enable-annot-syntax)

@url GET "/"
(defun index (params)
  (${application-name}.view.emb:render
   "index.emb"
   params))

@url POST "/"
(defun index-post (params)
  @ignore params
  "Hello, Caveman!")
