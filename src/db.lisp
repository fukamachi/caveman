(in-package :cl-user)
(defpackage caveman2.db
  (:use :cl)
  (:import-from :caveman.middleware.dbimanager
                :connect-db)
  (:import-from :dbi
                :prepare
                :execute
                :fetch-all
                :fetch)
  (:import-from :sxql
                :from
                :where
                :order-by
                :group-by
                :limit
                :offset
                :set=
                :left-join
                :union-queries
                :union-all-queries
                :yield
                :*quote-character*)
  (:export :connect-db
           :select-all
           :select-one
           :insert-into
           :update
           :delete-from
           :from
           :where
           :order-by
           :group-by
           :limit
           :offset
           :set=
           :left-join
           :union-queries
           :union-all-queries))
(in-package :caveman2.db)

(defmacro aprogn (&rest expressions)
  `(let* (,@(mapcar
              (lambda (expression) `(it ,expression))
              expressions))
     it))

(defun connection-quote-character (conn)
  (let ((package (package-name (symbol-package (type-of conn)))))
    (cond
      ((string= package #.(string :dbd.mysql)) #\`)
      ((string= package #.(string :dbd.postgres)) #\")
      ((string= package #.(string :dbd.sqlite3)) #\"))))

(defmacro execute-sxql (db fn field &body clauses)
  (let ((sql (gensym "SQL"))
        (bind (gensym "BIND")))
    `(let ((*quote-character* (or *quote-character*
                                  (connection-quote-character ,db))))
       (multiple-value-bind (,sql ,bind)
           (yield (,fn ,field ,@clauses))
         (aprogn
           (dbi:prepare ,db ,sql)
           (apply #'dbi:execute it ,bind))))))

(defmacro select-all (db field &body clauses)
  `(dbi:fetch-all (execute-sxql ,db sxql:select ,field ,@clauses)))

(defmacro select-one (db field &body clauses)
  `(dbi:fetch (execute-sxql ,db sxql:select ,field ,@clauses)))

(defmacro insert-into (db table &body clauses)
  `(execute-sxql ,db sxql:insert-into ,table ,@clauses))

(defmacro update (db table &body clauses)
  `(execute-sxql ,db sxql:update ,table ,@clauses))

(defmacro delete-from (db table &body clauses)
  `(execute-sxql ,db sxql:delete-from ,table ,@clauses))
