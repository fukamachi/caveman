#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman.view.emb
  (:use :cl)
  (:import-from :cl-emb
                :execute-emb))
(in-package :caveman.view.emb)

(cl-syntax:use-syntax :annot)

@export
(defun render (file params)
  "Render function for CL-EMB templates."
  (emb:execute-emb file :env params))
