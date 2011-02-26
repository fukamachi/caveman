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
  (:import-from :cl-fad
                :file-exists-p)
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
(defvar *init-file* #p"init.lisp")

@export
(defvar *server-type* :hunchentoot)

@export
(defvar *acceptor* nil)

@export
(defun start (&key (port 8080) debug lazy)
  (when *init-file*
    (let ((init-file (merge-pathnames *init-file* *application-root*)))
      (when (file-exists-p init-file)
        (load init-file))))
  (setf *clack-builder-lazy-p* lazy)
  (database-setup)
  (let ((app (if *static-directory*
                 (builder
                  (<clack-middleware-static>
                   :path (merge-pathnames *static-directory* *application-root*))
                  #'routing)
                 #'routing)))
    (setf *acceptor*
          (clackup app :port port :debug debug :server *server-type*))))

@export
(defun stop ()
  (funcall
   (intern "STOP"
           (concatenate 'string
                        "CLACK.HANDLER."
                        (symbol-name *server-type*)))
   *acceptor*))
