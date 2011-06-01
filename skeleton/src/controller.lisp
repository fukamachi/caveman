(clack.util:namespace ${application-name}.controller
  (:use :cl
        :caveman
        :${application-name})
  (:import-from :${application-name}.view.emb
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
