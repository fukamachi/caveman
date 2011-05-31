#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.config
  (:use :cl
        :clack)
  (:import-from :caveman.skeleton
                :slurp-file)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax))

(use-syntax annot-syntax)

@export
(defvar *config* nil
  "Special variable to store Caveman Config, a plist.
Don't set to this variable directory. This is designed to be bound in lexical let.")

@export
(defmethod load-config (app-name mode)
  (let ((config-file (asdf:system-relative-pathname
                      app-name
                      (format nil "src/config/~(~A~).lisp" mode))))
    (when (file-exists-p config-file)
      (eval
       (read-from-string
        ;; FIXME: remove dependence on skeleton, slurp-file.
        (caveman.skeleton::slurp-file config-file))))))

@export
(defun config (&optional key)
  (if key (getf *config* key) *config*))

(doc:start)

@doc:NAME "
Caveman.Config - Managing configuration for the running project.
"

@doc:SYNOPSIS "
    ;; get the current configuration as a plist.
    (config)
    
    ;; read the value from configuration.
    (config :static-path)
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
