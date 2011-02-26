#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static)
  (:import-from :cl-annot.doc
                :doc)
  (:import-from :caveman.route
                :routing)
  (:import-from :caveman.model
                :database-setup))

(cl-annot:enable-annot-syntax)

@export
(defvar *application-name* "Caveman")

@export
(defvar *application-root* #p"./")

@doc "
Static directory pathname.
This must ends with slash('/').
"
@export
(defvar *static-directory* #p"public/")

@export
(defun start (&key (port 8080) debug lazy)
  (setf *clack-builder-lazy-p* lazy)
  (database-setup)
  (let ((app (if *static-directory*
                 (builder
                  (<clack-middleware-static>
                   :path *static-directory*)
                  #'routing)
                 #'routing)))
    (clackup app :port port :debug debug)))
