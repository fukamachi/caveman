(clack.util:namespace ${application-name}.controller
  (:use :cl
        :caveman
        :${application-name}))

(cl-annot:enable-annot-syntax)

@url GET "/"
(defun index (params)
  @ignore params
  "Hello, Caveman!")
