#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.emb
  (:use :cl)
  (:import-from :cl-emb
                :execute-emb)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax))

(use-syntax annot-syntax)

@export
(defun render (file params)
  "Render function for CL-EMB templates."
  (emb:execute-emb file :env params))
