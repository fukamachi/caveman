#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.app
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static
        :clack.middleware.clsql)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :clack.request
                :make-request)
  (:import-from :caveman.database
                :database-setup)
  (:import-from :clsql
                :connect))

(cl-annot:enable-annot-syntax)

@export
(defclass <app> (<component>)
     ((config :initarg :config :initform nil
              :accessor config)
      (routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules)))

@export
(defmethod setup ((this <app>))
  (let ((config (config this)))
    (when (getf config :config-file)
      (let ((config-file (merge-pathnames (getf config :config-file)
                                        (getf config :application-root))))
        (when (file-exists-p config-file)
          (load config-file))))
    (database-setup (getf config :database-type)
                    (getf config :database-connection-spec))))

@export
(defmethod start ((this <app>)
                  &key port server debug lazy)
  (setup this)
  (setf *builder-lazy-p* lazy)
  (clackup (builder
            (<clack-middleware-static>
             :path "/public/"
             :root (merge-pathnames (getf (config this) :static-path)
                                    (getf (config this) :application-root)))
            (<clack-middleware-clsql>
             :database-type (getf (config this) :database-type)
             :connection-spec (getf (config this) :database-connection-spec)
             :connect-args '(:pool t))
            this)
           :port (or port (getf (config this) :port))
           :debug debug
           :server (or server (getf (config this) :server))))

(defmethod call ((this <app>) req)
  "Dispatch HTTP request to each actions."
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
                     (setf (slot-value req 'clack.request::query-parameters)
                           (append
                            params
                            (slot-value req 'clack.request::query-parameters)))
                     (connect (getf (config this) :database-connection-spec)
                              :database-type (getf (config this)
                                                   :database-type)
                              :pool t
                              :encoding :utf-8)
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
