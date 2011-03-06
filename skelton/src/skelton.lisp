(clack.util:namespace ${application-name}
  (:use :cl)
  (:import-from :caveman
                :<app>
                :add-route)
  (:import-from :caveman.route
                :url->routing-rule)
  (:import-from :cl-annot.core
                :annotation-narg))

(cl-annot:enable-annot-syntax)

@export
(defvar *acceptor* nil)

@export
(defvar *config*
    '(:application-name "${application-name}"
      :application-root ${application-root}
      :static-path #p"public/"
      :server :hunchentoot
      :port 8080
      :database-type :sqlite3
      :database-connection-spec ("memory")
      :config-file #p"config.lisp"))

@export
(defclass ${application-name} (<app>) ())

@export
(defvar *app* (make-instance '${application-name}
                 :config *config*))

@export
(defmacro url (method url-rule form)
  `(progn
     ,form
     (add-route *app*
                (url->routing-rule ,method ,url-rule ,form))))
(setf (annotation-narg 'url) 3)

@export
(defun start (&key debug)
  (caveman:start *app* :debug debug))
