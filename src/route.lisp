#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.route
  (:use :cl
        :clack)
  (:import-from :clack.app.route
                :url-rule->regex)
  (:import-from :cl-annot.util
                :progn-last
                :definition-symbol
                :definition-type)
  (:import-from :cl-annot.doc
                :doc)
  (:import-from :cl-annot.core
                :annotation-narg)
  (:import-from :caveman.app
                :add-route))

(cl-annot:enable-annot-syntax)

@doc "
Useful annotation to define actions.

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
    )
"
@export
(defmacro url (method url-rule form)
  `(progn
     ,form
     (add-route ,(intern "*APP*" *package*)
                (url->routing-rule ,method ,url-rule ,form))))
(setf (annotation-narg 'url) 3)

@doc "
Convert action form into a routing rule, a list.

Example:
  ((member-profile GET (\"^\\\\/member\\\\/(.+?)$\" (id)) #'member-profile)
   (login-form POST (\"^\\\\/login$\" nil) #'login-form))
"
@export
(defmacro url->routing-rule (method url-rule form)
  (let* ((last-form (progn-last form))
         (type (definition-type last-form))
         (symbol (definition-symbol last-form))
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
