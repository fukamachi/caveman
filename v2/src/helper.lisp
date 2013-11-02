(in-package :cl-user)
(defpackage caveman2.helper
  (:use :cl)
  (:import-from :caveman2.app
                :*response*)
  (:export :redirect))
(in-package :caveman2.helper)

(defun redirect (url &optional (status 302))
  (clack.response:redirect *response* url status))
