#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.route
  (:use :cl
        :clack
        :cl-annot
        :cl-annot.doc)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax)
  (:import-from :clack.util
                :getf*
                :remf*)
  (:import-from :clack.util.hunchentoot
                :url-encode)
  (:import-from :clack.util.route
                :make-url-rule)
  (:import-from :cl-annot.util
                :progn-form-last
                :definition-form-symbol
                :definition-form-type)
  (:import-from :caveman.app
                :add-route
                :lookup-route))

(use-syntax annot-syntax)

@export
(defannotation url (method url-rule form)
    (:arity 3)
  "Useful annotation to define actions.

Example:
  ;; for Function
  @url GET \"/login\"
  (defun login (req)
    ;; response
    )

  ;; for Clack Component
  @url GET \"/member/:id\"
  (defclass <member-profile> (<component>) ())
  (defmethod call ((this <member-profile>) req)
    ;; response
    )"
  `(progn
     (add-route ,(intern "*APP*" *package*)
                (url->routing-rule ,method ,url-rule ,form))
     ,form))

@doc "
Convert action form into a routing rule, a list.

Example:
  ((member-profile GET (\"^\\\\/member\\\\/(.+?)$\" (id)) #'member-profile)
   (login-form POST (\"^\\\\/login$\" nil) #'login-form))
"
@export
(defmacro url->routing-rule (method url-rule form)
  (let* ((last-form (progn-form-last form))
         (type (definition-form-type last-form))
         (symbol (definition-form-symbol last-form))
         (req (gensym "REQ")))
    `(list
      ',symbol
      ',method
      (make-url-rule ,url-rule)
      #'(lambda (,req)
          (call ,(if (eq type 'defclass)
                     `(make-instance ',symbol)
                     `(symbol-function ',symbol))
                ,req)))))

(defun add-query-parameters (base-url params)
  "Add a query parameters string of PARAMS to BASE-URL."
  (unless params
    (return-from add-query-parameters base-url))
  (loop for (name value) on params by #'cddr
        collect (format nil "~A=~A"
                        (url-encode (princ-to-string name))
                        (url-encode (princ-to-string value)))
        into parts
        finally
     (return
       (let ((params-string (format nil "~{~A~^&~}" parts)))
         (format nil "~A~A~A"
                 base-url
                 (if (find #\? base-url) "&" "?")
                 params-string)))))

@doc "
Make an URL for the action with PARAMS.

Example:
  @url GET \"/animals/:type\"
  (defun animals (params))

  (link-to 'animals :type \"cat\")
  ;; => \"/animals/cat\"
"
@export
(defun link-to (symbol &rest params)
  (let* ((package (symbol-package symbol))
         (app (symbol-value (find-symbol "*APP*" package)))
         (route (lookup-route app symbol)))
    (unless route
      (error "Route not found for ~A" symbol))
    (multiple-value-bind (base-url rest-params)
        (clack.util.route:link-to (third route) params)
      (add-query-parameters base-url rest-params))))

(doc:start)

@doc:NAME "Caveman.Route"

@doc:SYNOPSIS "
    ;; for Function
    @url GET \"/login\"
    (defun login (req)
      ;; response
      )
    
    ;; for Clack Component
    @url GET \"/member/:id\"
    (defclass <member-profile> (<component>) ())
    (defmethod call ((this <member-profile>) req)
      ;; response
      )
"

@doc:DESCRIPTION "
Caveman.Route provides an useful annotation `url' to define a Caveman Action.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
