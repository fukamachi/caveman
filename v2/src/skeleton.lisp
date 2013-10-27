#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :caveman2.skeleton
  (:use :cl)
  (:export :make-project))
(in-package :caveman2.skeleton)

(defvar *skeleton-directory*
  (asdf:system-relative-pathname :caveman2 #p"v2/skeleton/"))

(defun make-project (path &rest params &key name description author email license &allow-other-keys)
  (declare (ignore name description author email license))
  (let ((cl-project:*skeleton-directory* *skeleton-directory*))
    (apply #'cl-project:make-project path params)))
