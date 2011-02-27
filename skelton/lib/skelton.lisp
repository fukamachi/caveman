(in-package :cl-user)
(defpackage caveman.${application-name}
  (:use :cl)
  (:import-from :caveman
                :<app>
                :add-route)
  (:import-from :caveman.route
                :url->routing-rule)
  (:import-from :cl-annot.core
                :annotation-narg))
(in-package :caveman.${application-name})

(cl-annot:enable-annot-syntax)

@export
(defvar *acceptor* nil)

@export
(defclass ${application-name} (<app>) ())

@export
(defvar *app* (make-instance '${application-name}
                 :name "${application-name}"))

@export
(defmacro url (method url-rule form)
  `(add-route *app*
              (url->routing-rule ,method ,url-rule ,form)))
(setf (annotation-narg 'url) 3)
