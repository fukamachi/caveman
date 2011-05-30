#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.context
  (:use :cl
        :cl-annot.doc)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax)
  (:import-from :caveman.request
                :make-request)
  (:import-from :caveman.response
                :make-response))

(use-syntax annot-syntax)

@export
(defvar *context* nil
  "Special variable to store Caveman Context, a hash table.
Don't set to this variable directly. This is designed to be bound in lexical let.")

@export
(defvar *request* nil
  "Special variable to store Caveman Request, a instance of `<request>' in Caveman.Request package.
Don't set to this variable directly. This is designed to be bound in lexical let.")

@export
(defvar *response* nil
  "Special variable to store Caveman Response, a instance of `<response>' in Caveman.Response package.
Don't set to this variable directly. This is designed to be bound in lexical let.")

@export
(defvar *session* nil
  "Special variable to store session.
Don't set to this variable directly. This is designed to be bound in lexical let.")

@doc "Create a new Context."
@export
(defun make-context (req)
  (let ((*context* (make-hash-table)))
    (setf (context :request) (make-request req)
          (context :response) (make-response 200 ())
          (context :session) (getf req :clack.session))
    *context*))

@doc "
Access to current context. If key is specified, return the value in current context.
If not, just return a current context.

Example:
  (context)
  ;=> #<HASH-TABLE :TEST EQL size 0/60 #x3020025FF5FD>
  (context :request)
  ;=> #<CAVEMAN.REQUEST:<REQUEST> #x3020024FCCFD>
"
@export
(defun context (&optional key)
  (if key (gethash key *context*) *context*))

@export
(defun (setf context) (val key)
  (setf (gethash key *context*) val))

@export
(defmacro with-context-variables ((&rest vars) &body body)
  `(symbol-macrolet
       ,(loop for var in vars
              for form = `(context ,(intern (string var) :keyword))
              collect `(,var ,form))
     ,@body))

(doc:start)

@doc:NAME "
Caveman.Context - Managing current state for each request.
"

@doc:SYNOPSIS "
    ;; In the controller.
    
    ;; Get context object.
    (context)
    ;=> #<HASH-TABLE :TEST EQL size 0/60 #x3020025FF5FD>
    
    ;; Access to each value.
    (context :hoge)
    ;=> \"Something set to :hoge.\"
    
    ;; Set to context
    (setf (context :hoge) \"hogehoge\")
"

@doc:DESCRIPTION "
Caveman.Context is for managing current state for each request. It is called \"Context\" in Caveman.

Specifically, context is a hash table in global scope. you can access it with a function `context'. See SYNOPSIS for details.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
