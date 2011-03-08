(clack.util:namespace ${application-name}
  (:use :cl
        :clack)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>
                :add-route
                :config)
  (:import-from :caveman.route
                :url->routing-rule)
  (:import-from :caveman.context
                :request*
                :response*)
  (:import-from :caveman.middleware.context
                :<caveman-middleware-context>)
  (:import-from :cl-annot.core
                :annotation-narg))

(cl-annot:enable-annot-syntax)

@export
(defvar *context* nil)

(defvar *acceptor* nil)

@export
(defclass ${application-name} (<app>) ())

(defmethod call :around ((this ${application-name}) req)
  (let ((mw (make-instance '<caveman-middleware-context>
               :context '*context*)))
    (call (wrap mw #'(lambda (req)
                       (call-next-method this req)))
          req)))

@export
(defvar *app* (make-instance '${application-name}
                 :config
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
                   :config-file #p"config.lisp")))

@export
(defmacro url (method url-rule form)
  `(progn
     ,form
     (add-route *app*
                (url->routing-rule ,method ,url-rule ,form))))
(setf (annotation-narg 'url) 3)

@export
(defun start (&key debug)
  (setf *acceptor*
        (caveman.app:start *app* :debug debug)))

@export
(defun stop ()
  (clack:stop *acceptor* :server (getf (config *app*) :server))
  (setf *acceptor* nil))

@export
(defun request ()
  (request* *context*))

@export
(defun response ()
  (response* *context*))
