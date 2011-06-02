#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.configloader
  (:use :cl)
  (:import-from :cl-fad
                :file-exists-p))

(cl-annot:enable-annot-syntax)

@export
(defun load-config (app &optional mode)
  (let ((config-file (asdf:system-relative-pathname
                      (type-of app)
                      (format nil "config/~(~A~).lisp" mode))))
    (when (file-exists-p config-file)
      (eval
       (read-from-string
        ;; FIXME: remove dependence on skeleton, slurp-file.
        (caveman.skeleton::slurp-file config-file))))))

(doc:start)

@doc:NAME "
Caveman.ConfigLoader - Simple configuration loader.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
