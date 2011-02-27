(in-package :cl-user)
(defpackage ${application-name}.config
  (:use :cl
        :caveman.${application-name}
        :caveman.model))
(in-package :${application-name}.config)

(setf (application-name *app*) ${application-name})
(setf (application-root *app*) ${application-root})
(setf *database-type* :sqlite3)
(setf *database-connection-spec* '("db/sqlite3.db"))
