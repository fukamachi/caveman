#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage caveman.project
  (:use :cl
        :anaphora
        :clack
        :clack.builder)
  (:shadow :stop)
  (:import-from :caveman.context
                :*project*)
  (:export :debug-mode-p
           :project-mode
           :config))
(in-package :caveman.project)

(cl-syntax:use-syntax :annot)

@export
(defclass <project> (<component>)
     ((config :initarg :config :initform nil
              :accessor config)
      (acceptor :initform nil :accessor acceptor)
      (debug-mode-p :type boolean
                    :initarg :debug-mode-p
                    :initform t
                    :accessor debug-mode-p)
      (mode :type keyword
            :initarg :mode
            :accessor project-mode)))

@export
(defgeneric build (project)
  (:documentation "Build up an application for this project and return it. This method must be implemented in subclasses."))

(defun slurp-file (path)
  "Read a specified file and return the content as a sequence."
  (with-open-file (stream path :direction :input)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

@export
(defmethod load-config ((this <project>) mode)
  (let ((config-file (asdf:system-relative-pathname
                      (intern
                       (package-name (symbol-package (type-of this)))
                       :keyword)
                      (format nil "config/~(~A~).lisp" mode))))
    (when (probe-file config-file)
      (eval
       (read-from-string
        (slurp-file config-file))))))

@export
(defmethod start :around ((this <project>) &key (mode :dev) debug lazy &allow-other-keys)
  (let ((*project* this)
        (config (load-config this mode)))
    (setf (config this) config)
    (ensure-directories-exist
     (merge-pathnames (getf config :log-path)
                      (getf config :application-root)))
    (setf (project-mode this) mode)
    (setf (debug-mode-p this) debug)
    (setf *builder-lazy-p* lazy)
    (call-next-method)))

@export
(defmethod start ((this <project>) &key port server &allow-other-keys)
  (let ((app (build this))
        (config (config this)))
    (setf (acceptor this)
          (clackup
           (lambda (env) (let ((*project* this)) (call app env)))
           :port (or port (getf config :port))
           :debug (debug-mode-p this)
           :server (or server (getf config :server))))))

@export
(defmethod stop ((this <project>))
  "Stop a server."
  (swhen (acceptor this)
    (clack:stop it)
    (setf it nil)))
