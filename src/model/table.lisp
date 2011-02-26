#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.model.table
  (:use :cl)
  (:import-from :clsql
                :enable-sql-reader-syntax
                :create-view-from-class
                :select
                :sql-expression
                :sql-operator
                :table-exists-p
                :sql-and
                :sql-=
                :sql-in
                :sql-count)
  (:import-from :clsql-sys :standard-db-class)
  (:import-from :caveman.model.record
                :<caveman-record>
                :save))

(cl-annot:enable-annot-syntax)

@export
(defclass <caveman-table> (clsql-sys::standard-db-class) ()
  (:documentation "Metaclass for database tables."))

@export
(defmethod create-instance ((table symbol) &rest initargs)
  (apply #'create-instance (find-class table) initargs))

@export
(defmethod create-instance ((table <caveman-table>) &rest initargs)
  "Same as `make-instance' except for calling `save' it then.

Example:
  (create-instance 'person :name \"Eitarow Fukamachi\")"
  (let ((new-instance (apply #'make-instance table initargs)))
    (save new-instance)
    new-instance))

(defun remove-nil-from-plist (plist)
  (loop for (k v) on plist by #'cddr
        unless (eq v nil)
          append (list k v)))

@export
(defmethod fetch ((table symbol) ids-or-key
                  &key where conditions order-by offset limit group-by)
  (fetch (find-class table) ids-or-key
         :where where
         :conditions conditions
         :order-by order-by
         :offset offset
         :limit limit
         :group-by group-by))

@export
(defmethod fetch ((table <caveman-table>) ids-or-key
                  &key where conditions order-by offset limit group-by)
  "Find records from `table' and return it.
`ids-or-key' must be :first, :all, or a number, represents primary key, or the list.

Example:
  ;; Fetch a record, id=1.
  (fetch person 1)
  ;; Fetch records, country=jp
  (fetch person :conditions '(:country \"jp\"))"
  (setf table (class-name table))
  (etypecase ids-or-key
    (keyword (ecase ids-or-key
               (:first
                (car
                 (apply #'select table :flatp t
                        (remove-nil-from-plist
                         `(:limit 1
                           :offset ,offset
                           :order-by ,(normalize-order-by order-by)
                           :group-by ,group-by
                           :where ,(cond
                                     ((and where conditions)
                                      (sql-and where (normalize-conditions conditions)))
                                     (where where)
                                     (conditions (normalize-conditions conditions))))))))
               (:count
                (car
                 (apply #'select (sql-count (sql-expression :attribute "*")) :from table :flatp t
                  (remove-nil-from-plist
                   `(:offset ,offset
                     :order-by ,(normalize-order-by order-by)
                     :group-by ,group-by
                     :where ,(cond
                               ((and where conditions)
                                (sql-and where (normalize-conditions conditions)))
                               (where where)
                               (conditions (normalize-conditions conditions))))))))
               (:all
                (apply #'select table :flatp t
                        (remove-nil-from-plist
                         `(:limit ,limit
                           :offset ,offset
                           :order-by ,(normalize-order-by order-by)
                           :group-by ,group-by
                           :where ,(cond
                                     ((and where conditions)
                                      (sql-and where (normalize-conditions conditions)))
                                     (where where)
                                     (conditions (normalize-conditions conditions)))))))))
    ((or number string)
     (car
      (apply #'select table
             :where
             (cond
               ((and where conditions)
                (sql-and (sql-= (sql-expression :attribute "id") ids-or-key) where (normalize-conditions conditions)))
               (where (sql-and (sql-= (sql-expression :attribute "id") ids-or-key) where))
               (conditions (sql-and (sql-= (sql-expression :attribute "id") ids-or-key) (normalize-conditions conditions)))
               (t (sql-= (sql-expression :attribute "id") ids-or-key)))
             :flatp t
             (remove-nil-from-plist
              `(:order-by ,(normalize-order-by order-by) :group-by ,group-by)))))
    (cons
     (apply #'select table
            :where
            (if where
                (sql-and (sql-in (sql-expression :attribute "id") ids-or-key) where)
                (sql-in (sql-expression :attribute "id") ids-or-key))
            :flatp t
            (remove-nil-from-plist
             `(:limit ,limit
               :offset ,offset
               :order-by ,(normalize-order-by order-by)
               :group-by ,group-by))))))

@export
(defmacro deftable (class supers slots &optional cl-options)
  "Define a table schema. This is just a wrapper of `clsql:def-view-class',
so, see CLSQL documentation to get more informations.
<http://clsql.b9.com/manual/def-view-class.html>"
  `(progn
     (clsql:def-view-class ,class (<caveman-record> ,@supers)
      ,slots
      ,@(if (find :metaclass `,cl-options :key #'car)
            `,cl-options
            (cons '(:metaclass <caveman-table>) `,cl-options)))
;     (unless (table-exists-p ',class)
;       (create-view-from-class ',class))
     (defvar ,class (find-class ',class))))

(defun normalize-conditions (conditions)
  (apply #'sql-and
         (loop for (k v) on conditions by #'cddr
               collect (sql-= (sql-expression :attribute k) v))))

(defun normalize-order-by (order-by)
  (loop for (k v) on order-by by #'cddr
        collect (list (sql-expression :attribute k) v)))
