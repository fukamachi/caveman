#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.model
  (:use :cl)
  (:import-from :caveman.model.table
                :<caveman-table>
                :deftable
                :create-instance
                :fetch)
  (:import-from :caveman.model.record
                :<caveman-record>
                :save
                :destroy
                :attributes)
  (:import-from :clsql
                :*default-caching*
                :*db-auto-sync*
                :connect
                :list-classes
                :table-exists-p
                :create-view-from-class)
  (:export :<caveman-table>
           :deftable
           :create-instance
           :fetch
           :<caveman-record>
           :save
           :destroy
           :attributes))

(cl-annot:enable-annot-syntax)

@export
(defvar *database-type* :sqlite3)

@export
(defvar *database-connection-spec* '("db/test.sqlite3"))

@export
(defun database-setup ()
  (setf *default-caching* nil)
  (setf *db-auto-sync* nil)
  (connect *database-connection-spec*
                 :database-type *database-type*
                 :pool t
                 :encoding :utf-8)
  (dolist (class (list-classes))
    (unless (table-exist-p (class-name class))
      (create-view-from-class class))))
