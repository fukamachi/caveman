(clack.util:namespace <% @var name %>
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.clsql)
  (:shadow :stop)
  (:import-from :caveman.project
                :<project>
                :build)
  (:import-from :<% @var name %>.app
                :*app*))

(cl-annot:enable-annot-syntax)

@export
(defclass <<% @var name %>> (<project>) ())

@export
(defvar *project* nil)

(defmethod build ((this <<% @var name %>>) &optional app)
  @ignore app
  (call-next-method
   this
   (builder
    (<clack-middleware-clsql>
     :database-type (getf (caveman.project:config this)
                          :database-type)
     :connection-spec (getf (caveman.project:config this)
                            :database-connection-spec)
     :connect-args '(:pool t :encoding :utf-8))
    <% @var name %>.app:*app*)))

@export
(defun start (&key (mode :dev) (debug t) lazy)
  (setf *project* (make-instance '<<% @var name %>>))
  (caveman.project:start *project* :mode mode :debug debug :lazy lazy))

@export
(defun stop ()
  (when *project*
    (caveman.project:stop *project*)
    (setf *project* nil)))
