(clack.util:namespace ${application-name}
  (:use :cl
        :clack)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>
                :config))

(cl-annot:enable-annot-syntax)

@export
(defclass ${application-name} (<app>) ())

@export
(defvar *app* (make-instance '${application-name}))

@export
(defvar *config*
    `(:application-name "${application-name}"
      :application-root ,(asdf:component-pathname
                          (asdf:find-system :${application-name}))
      :static-path #p"public/"
      :server :hunchentoot
      :port 8080
      :database-type :sqlite3
      :database-connection-spec (,(namestring
                                   (asdf:system-relative-pathname
                                    :${application-name}
                                    "sqlite3.db")))
      :config-file #p"config.lisp"))

@export
(defun start (&key debug)
  (setf (slot-value *app* 'config) *config*)
  (caveman.app:start *app* :debug debug))

@export
(defun stop ()
  (caveman.app:stop *app*))
