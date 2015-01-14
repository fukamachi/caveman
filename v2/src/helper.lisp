(in-package :cl-user)
(defpackage caveman2.helper
  (:use :cl)
  (:import-from :caveman2.app
                :*current-app*
                :*response*)
  (:import-from :ningle.app
                :mapper)
  (:import-from :myway
                :mapper-routes
                :route-name
                #+nil :url-for)
  (:export :redirect
           :url-for))
(in-package :caveman2.helper)

(defun redirect (url &optional (status 302))
  (clack.response:redirect *response* url status))

(defun add-query-parameters (base-url params)
  "Add a query parameters string of PARAMS to BASE-URL."
  (unless params
    (return-from add-query-parameters base-url))
  (loop for (name value) on params by #'cddr
        collect (format nil "~A=~A"
                        (urlencode (princ-to-string name))
                        (urlencode (princ-to-string value)))
        into parts
        finally
     (return
       (let ((params-string (format nil "~{~A~^&~}" parts)))
         (format nil "~A~A~A"
                 base-url
                 (if (find #\? base-url) "&" "?")
                 params-string)))))

(defun url-for (route-name &rest params)
  (let ((route (find-if #'(lambda (route)
                            (string-equal (route-name route) route-name))
                        (mapper-routes (mapper *current-app*)))))
    (if route
        (multiple-value-bind (base-url rest-params)
            (myway:url-for route params)
          (add-query-parameters base-url rest-params))
        (error "No route found for ~S" route-name))))
