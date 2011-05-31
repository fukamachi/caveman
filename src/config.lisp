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
(defmethod load-config (app-name mode)
  (let ((config-file (asdf:system-relative-pathname
                      app-name
                      (format nil "src/config/~(~A~).lisp" mode))))
    (when (file-exists-p config-file)
      (eval
       (read-from-string
        ;; FIXME: remove dependence on skeleton, slurp-file.
        (caveman.skeleton::slurp-file config-file))))))
