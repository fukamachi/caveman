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
                :doc))

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
    
    (clackup #'routing)
"

@doc:DESCRIPTION "
Routing rules. This is refered by caveman.app:<app>.
Usually, you don't have to change it directly.

Example:
  ((member-profile GET (\"^\\\\/member\\\\/(.+?)$\" (id)) #'member-profile)
   (login-form POST (\"^\\\\/login$\" nil) #'login-form))
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
