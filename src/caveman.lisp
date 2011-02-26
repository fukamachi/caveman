#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman
  (:use :cl
        :clack)
  (:import-from :caveman.route
                :routing))

(cl-annot:enable-annot-syntax)

@export
(defun start (&key (port 8080) debug)
  (clackup #'routing :port port :debug debug))
