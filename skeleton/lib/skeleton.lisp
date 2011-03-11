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
(defun start (&key debug)
  (caveman.app:start *app* :debug debug))

@export
(defun stop ()
  (caveman.app:stop *app*))
