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
  (:import-from :clack.app.route
                :url-rule->regex)
  (:import-from :cl-annot.util
                :progn-form-last
                :definition-form-symbol
                :definition-form-type)
  (:import-from :caveman.app
                :add-route))

(cl-annot:enable-annot-syntax)

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
      (url-rule->regex ,url-rule)
      #'(lambda (,req)
          (call ,(if (eq type 'defclass)
                     `(make-instance ',symbol)
                     `(symbol-function ',symbol))
                ,req)))))

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
