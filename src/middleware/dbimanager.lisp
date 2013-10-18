(in-package :cl-user)
(defpackage caveman.middleware.dbimanager
  (:use :cl
        :clack)
  (:import-from :dbi
                :connect
                :disconnect
                :ping)
  (:export :<caveman-middleware-dbimanager>
           :connect-db))
(in-package :caveman.middleware.dbimanager)

(defvar *dbi-manager* nil
  "An instance of `dbi-manager' for the current HTTP request.
Since this variable meant to be bound lexically, this is available only during a HTTP request.")

(defclass dbi-manager ()
  ((database-settings :type list
                      :initarg :database-settings)
   (connections :initform (make-hash-table :test 'eql)))
  (:documentation "Class for managing CL-DBI connections."))

(defclass <caveman-middleware-dbimanager> (<middleware>)
  ((dbi-manager :type dbi-manager))
  (:documentation "Clack Middleware for using CL-DBI.
This is similar to `<clack-middleware-dbi>', except this doesn't always connect (and disconnect) for each HTTP requests."))

(defmethod initialize-instance :after ((this <caveman-middleware-dbimanager>) &key database-settings)
  (setf (slot-value this 'dbi-manager)
        (make-instance 'dbi-manager
                       :database-settings database-settings)))

(defmethod call ((this <caveman-middleware-dbimanager>) env)
  (let ((*dbi-manager* (slot-value this 'dbi-manager)))
    (call-next this env)))

(defmethod get-connection ((manager dbi-manager) &optional database-name)
  "Return a connected DBI connection for a database for `database-name'.
If `database-name' is NIL, the first one in database settings will be adopted."
  (unless database-name
    (setf database-name (car (default-database-setting manager))))

  (unless database-name
    (error "No database settings in the dbi-manager."))

  (symbol-macrolet ((conn (gethash database-name (slot-value manager 'connections))))
    (cond
      ((not conn)
       (setf conn
             (apply #'dbi:connect
                    (cdr (database-setting manager database-name))))
       conn)
      ((not (dbi:ping conn))
       (dbi:disconnect conn)
       (setf conn nil)
       (get-connection manager database-name))
      (T conn))))

(defun connect-db (&optional database-name)
  "Return a connected DBI connection for database for `database-name'.
This is meant to be used in an actual application."
  (get-connection *dbi-manager* database-name))

(defmethod default-database-setting ((manager dbi-manager))
  (first (slot-value manager 'database-settings)))

(defmethod database-setting ((manager dbi-manager) database-name)
  (assoc database-name (slot-value manager 'database-settings)))
