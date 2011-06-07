(clack.util:namespace <% @var name %>
  (:use :cl
        :clack
        :clack.builder)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>)
  (:import-from :caveman.configloader
                :load-config))

(cl-annot:enable-annot-syntax)

@export
(defclass <% @var name %> (<app>) ())

@export
(defvar *app* (make-instance '<% @var name %>))

(defmethod build ((this <% @var name %>) app)
  (call-next-method
   this
   (builder
    (<clack-middleware-clsql>
     :database-type (getf (caveman.app:config this)
                          :database-type)
     :connection-spec (getf (caveman.app:config this)
                            :database-connection-spec)
     :connect-args '(:pool t :encoding :utf-8))
    <clack-middleware-session>
    app)))

@export
(defun start (&key (mode :dev) debug lazy)
  (setf (caveman.app:config *app*)
        (load-config *app* mode))
  (caveman.app:start *app* :mode mode :debug debug :lazy lazy))

@export
(defun stop ()
  (caveman.app:stop *app*))

@export
(defun config (&optional key)
  (let ((conf (caveman.app:config *app*)))
    (if key (getf conf key) conf)))
