(clack.util:namespace cavetest.action
  (:use :cl
        :${application-name}))

(cl-annot:enable-annot-syntax)

@url GET "/"
(defun index (params)
  @ignore params
  "Hello, Caveman!")
