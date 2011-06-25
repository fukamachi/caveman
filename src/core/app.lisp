#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.app
  (:use :cl
        :anaphora
        :clack
        :clack.builder
        :clack.middleware.static
        :clack.middleware.session
        :caveman.middleware.context)
  (:shadow :stop)
  (:import-from :local-time
                :format-timestring
                :now)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :clack.util.route
                :match)
  (:import-from :caveman.request
                :request-method
                :path-info
                :parameter)
  (:import-from :caveman.context
                :*app*
                :*request*
                :*response*)
  (:export :debug-mode-p
           :config))

(use-syntax annot-syntax)

@export
(defclass <app> (<component>)
     ((config :initarg :config :initform nil
              :accessor config)
      (routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules)
      (acceptor :initform nil :accessor acceptor)
      (debug-mode-p :type boolean
                    :initarg :debug-mode-p
                    :initform t
                    :accessor debug-mode-p))
  (:documentation "Base class for Caveman Application. All Caveman Application must inherit this class."))

(defmethod call ((this <app>) req)
  "Overriding method. This method will be called for each request."
  @ignore req
  (let* ((req *request*)
         (path-info (path-info req))
         (method (request-method req)))
    (loop for (nil rule fn) in (reverse (routing-rules this))
          do (multiple-value-bind (matchp params)
                 (match rule method path-info)
               (when matchp
                 (setf (slot-value req 'clack.request::query-parameters)
                       (append
                        params
                        (slot-value req 'clack.request::query-parameters)))
                 (let ((res (call fn (parameter req))))
                   (unless (eq res (next-route))
                     (return res)))))
          finally
          (progn (setf (clack.response:status *response*) 404)
                 nil))))

@export
(defmethod build ((this <app>) &optional (app this))
  (builder
   (clack.middleware.logger:<clack-middleware-logger>
    :logger (make-instance 'clack.logger.file:<clack-logger-file>
               :output-file
               (merge-pathnames
                (local-time:format-timestring nil
                 (local-time:now)
                 :format
                 '("log-" (:year 4) (:month 2) (:day 2)))
                (merge-pathnames
                 (getf (config this) :log-path)
                 (getf (config this) :application-root)))))
   (<clack-middleware-static>
    :path "/public/"
    :root (merge-pathnames (getf (config this) :static-path)
                           (getf (config this) :application-root)))
   <clack-middleware-session>
   <caveman-middleware-context>
   app))

@export
(defmethod add-route ((this <app>) routing-rule)
  "Add a routing rule to the Application."
  (setf (routing-rules this)
        (delete (car routing-rule)
                (routing-rules this)
                :key #'car))
  (push routing-rule
        (routing-rules this)))

(defparameter +next-route+ '#:next-route)

@export
(defun next-route ()
  +next-route+)

@export
(defmethod lookup-route ((this <app>) symbol)
  "Lookup a routing rule with SYMBOL from the application."
  (loop for rule in (reverse (routing-rules this))
        if (eq (first rule) symbol) do
          (return rule)))

@export
(defmethod start ((this <app>) &key (mode :dev) port server debug lazy)
  (let ((config (load-config this mode)))
    (setf (config this) config)
    (ensure-directories-exist
     (merge-pathnames (getf config :log-path)
                      (getf config :application-root)))
    (setf (debug-mode-p this) debug)
    (setf *builder-lazy-p* lazy)
    (let ((app (build this)))
      (setf (acceptor this)
            (clackup
             (lambda (env) (let ((*app* this)) (call app env)))
             :port (or port (getf config :port))
             :debug debug
             :server (or server (getf config :server)))))))

@export
(defmethod stop ((this <app>))
  "Stop a server."
  (swhen (acceptor this)
    (clack:stop it)
    (setf it nil)))

(defun slurp-file (path)
  "Read a specified file and return the content as a sequence."
  (with-open-file (stream path :direction :input)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

@export
(defmethod load-config ((this <app>) mode)
  (let ((config-file (asdf:system-relative-pathname
                      (type-of this)
                      (format nil "config/~(~A~).lisp" mode))))
    (when (file-exists-p config-file)
      (eval
       (read-from-string
        (slurp-file config-file))))))

(doc:start)

@doc:NAME "
Caveman.App - Caveman Application Class.
"

@doc:SYNOPSIS "
    ;; Usually you shouldn't write this code.
    ;; These code will be generated by `caveman.skeleton:generate'.
    (defclass <myapp> (<app>) ())
    (defvar *app* (make-instance '<myapp>
                     :config '(:application-name \"My App\"
                               :application-root #p\"~/public/\"
                               :server :hunchentoot
                               :port 8080)))
    (start *app*)
"

@doc:DESCRIPTION "
Caveman.App provide a base class `<app>' for Caveman Application.

Usually you don't have to cave about this package because `caveman.skeleton:generate' will generate code for you.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
"
