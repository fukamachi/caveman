#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.model.record
  (:use :cl)
  (:import-from :clsql
                :update-records-from-instance
                :delete-instance-records))

(cl-annot:enable-annot-syntax)

@export
(defclass <caveman-record> () ()
  (:documentation "Class for table instances, represents database record."))

@export
(defmethod save ((this <caveman-record>))
  "Save this instance and reflect slot values to database."
  (update-records-from-instance this))

@export
(defmethod destroy ((this <caveman-record>))
  "Delete this instance from the database.
I want to use `delete' for this name, but it is already used in very famous package :p."
  (delete-instance-records this))

@export
(defmethod attributes ((this <caveman-record>))
  "Return a list of slot names."
  (list-attributes this))
