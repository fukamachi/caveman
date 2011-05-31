#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.emb
  (:use :cl)
  (:import-from :caveman.view
                :render)
  (:import-from :cl-emb
                :execute-emb)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax))

(use-syntax annot-syntax)

(defmethod render ((this pathname) params)
  "Render method for CL-EMB templates."
  (emb:execute-emb this :env params))
