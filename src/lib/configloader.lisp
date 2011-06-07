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
        (slurp-file config-file))))))

(defun slurp-file (path)
  "Read a specified file and return the content as a sequence."
  (with-open-file (stream path :direction :input)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(doc:start)

@doc:NAME "
Caveman.ConfigLoader - Simple configuration loader.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
