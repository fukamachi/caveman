#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.database
  (:use :cl)
  (:import-from :clsql
                :*default-caching*
                :*db-auto-sync*
                :connect
                :disconnect
                :list-classes
                :table-exists-p
                :create-view-from-class))

(cl-annot:enable-annot-syntax)

@export
(defun clsql-database-setup (type spec)
  (setf *default-caching* nil)
  (setf *db-auto-sync* nil)
  (let ((db (connect spec
                     :database-type type
                     :make-default nil
                     :pool t
                     :encoding :utf-8)))
    (dolist (class (list-classes :database db))
      (unless (table-exist-p (class-name class)
                             :database db)
        (create-view-from-class class :database db)))
    (disconnect :database db)))

(doc:start)

@doc:NAME "
Caveman.Database - Setup database.
"

@doc:DESCRIPTION "
Caveman.Database is to setup database before an Application starts.

Currently supports only CLSQL.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* CLSQL
"
