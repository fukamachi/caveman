(clack.util:namespace ${application-name}
  (:use :cl
        :clack)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>))

(cl-annot:enable-annot-syntax)

@export
(defclass ${application-name} (<app>) ())

@export
(defvar *app* (make-instance '${application-name}))

@export
(defun start (&key (mode :dev) debug lazy)
  (caveman.app:start *app* :mode mode :debug debug :lazy lazy))

@export
(defun stop ()
  (caveman.app:stop *app*))

@export
(defun config ()
  (caveman.app:config *app*))
