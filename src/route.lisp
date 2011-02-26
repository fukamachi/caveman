#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.route
  (:use :cl
        :clack)
  (:shadowing-import-from :caveman.view :call)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :clack.request
                :make-request)
  (:import-from :clack.app.route
                :url-rule->regex)
  (:import-from :cl-annot.util
                :progn-last
                :definition-symbol
                :definition-type)
  (:import-from :cl-annot.core
                :annotation-narg)
  (:import-from :cl-annot.doc
                :doc))

@doc "
Routing rules. This is refered by a function `routing', under this.
Usually, you don't have to change this directly.

Example:
  ((member-profile GET (\"^\\\\/member\\\\/(.+?)$\" (id)) #'member-profile)
   (login-form POST (\"^\\\\/login$\" nil) #'login-form))
"
@export
(defvar *routing-rules* nil)

@doc "
Dispatch HTTP request to each actions.
This is written as a Clack Application.

Example:
  (clackup #'routing)
"
@export
(defun routing (req)
  (let ((method (getf req :request-method))
        (path-info (getf req :path-info)))
    (loop for rule in *routing-rules*
          for (meth (re vars) fn) = (cdr rule)
          if (string= meth method)
            do (multiple-value-bind (matchp res)
                   (scan-to-strings re path-info)
                 (when matchp
                   (let ((req (make-request req))
                         (params
                          (loop for key in vars
                                for val in (coerce res 'list)
                                append (list
                                         (intern (symbol-name key) :keyword)
                                         val))))
                     (setf (slot-value req 'clack.request:query-parameter)
                           (append
                            params
                            (slot-value req 'clack.request:query-parameter)))
                     (return (call fn req)))))
          finally (return '(404 nil nil)))))

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
  (let* ((last-form (progn-last form))
         (type (definition-type last-form))
         (symbol (definition-symbol last-form))
         (req (gensym "REQ")))
    `(progn
       (setf *routing-rules*
             (delete ',symbol *routing-rules* :key #'car))
       (push (list
              ',symbol
              ',method
              (url-rule->regex ,url-rule)
              #'(lambda (,req)
                  (call ,(if (eq type 'defclass)
                             `(make-instance ',symbol)
                             `(symbol-function ',symbol))
                        ,req)))
             *routing-rules*)
       ,form)))
(setf (annotation-narg 'url) 3)

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

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
