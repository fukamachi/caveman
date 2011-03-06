(clack.util:namespace cavetest.action
  (:use :cl
        :${application-name}))

(cl-annot:enable-annot-syntax)

@url GET "/"
(defun index (req)
  @ignore req
  '(200 (:content-type "text/plain") ("Hello, Caveman!")))
