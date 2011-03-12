(clack.util:namespace ${application-name}
  (:use :cl
        :clack)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>
                :config)
  (:export :config))

(cl-annot:enable-annot-syntax)

@export
(defclass ${application-name} (<app>) ())

@export
(defvar *app* (make-instance '${application-name}))

@export
(defvar *config* nil)

@export
(defvar *acceptor* nil)

@export
(defun start (&key debug lazy)
  (setf *acceptor*
        (caveman.app:start :${application-name} :debug debug :lazy lazy)))

@export
(defun stop ()
  (caveman.app:stop *acceptor* *config*))
