#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :clack.request
                :make-request)
  (:import-from :caveman.model
                :database-setup)
  (:import-from :cl-annot.doc
                :doc))

(cl-annot:enable-annot-syntax)

@export
(defclass <app> (<component>)
     ((name :initarg :name :initform ""
            :accessor application-name)
      (root :initarg :root :initform #p"./"
            :accessor application-root)
      (static-path :initarg :static-path :initform #p"public/"
                   :accessor static-path)
      (init-file :initarg :init-file :initform #p"init.lisp"
                 :accessor init-file)
      (port :initarg :port :initform 8080
            :accessor port)
      (server :initarg :server :initform :hunchentoot
              :accessor server)
      (database-type :initarg :database-type
                     :initform :sqlite3
                     :accessor database-type)
      (database-connection-spec :initarg :database-connection-type
                                :initform '("db.sqlite3"))
      (routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules)))

@export
(defmethod setup ((this <app>))
  (when (init-file this)
    (let ((init-file (merge-pathnames (init-file this)
                                      (application-root this))))
      (when (file-exists-p init-file)
        (load init-file))))
  (database-setup (database-type this)
                  (database-connection-spec this)))

@export
(defmethod start ((this <app>)
                  &key port server debug lazy)
  (setup this)
  (setf *builder-lazy-p* lazy)
  (clackup (builder
            (<clack-middleware-static>
             :path "/public/"
             :root (merge-pathnames (static-path this)
                                    (application-root this)))
            this)
           :port (or port (port this))
           :debug debug
           :server (or server (server this))))

@export
(defmethod call ((this <app>) req)
  "Dispatch HTTP request to each actions.
This returns a Clack Application."
  (let ((method (getf req :request-method))
        (path-info (getf req :path-info)))
    (loop for rule in (reverse (routing-rules this))
          for (meth (re vars) fn) = (cdr rule)
          if (string= meth method)
            do (multiple-value-bind (matchp res)
                   (scan-to-strings re path-info)
                 (when matchp
                   (let ((req (make-request req))
                         (params
                          (loop for key in vars
                                for val in (coerce res 'list)
                                append (list
                                         (intern (symbol-name key) :keyword)
                                         val))))
                     (setf (slot-value req 'clack.request:query-parameter)
                           (append
                            params
                            (slot-value req 'clack.request:query-parameter)))
                     (return (call fn req)))))
          finally (return '(404 nil nil)))))

@export
(defmethod add-route ((this <app>) routing-rule)
  (setf (routing-rules this)
        (delete (car routing-rule)
                (routing-rules this)
                :key #'car))
  (push routing-rule
        (routing-rules this)))
