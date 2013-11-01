#|
  This file is a part of caveman project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2-test
  (:use :cl
        :caveman2
        :cl-test-more))
(in-package :caveman2-test)

(plan nil)

(is-expand
 (defroute (*web* "/") ()
   (format nil "Welcome"))
 (setf (ningle:route *web* "/")
      (lambda ($params)
        (declare (ignorable $params))
        (progn (format nil "Welcome"))))
 "defroute with no name")

(is-expand
 (defroute index (*web* "/") ()
   (format nil "Welcome"))
 (prog1
     (defun index () (format nil "Welcome"))
   (setf (ningle:route *web* "/")
         (lambda ($params)
           (declare (ignorable $params))
           (funcall #'index))))
 "defroute with a name")

(is-expand
 (defroute (*web* "/hello/?:name?") (&key name)
   (format nil "Hello, ~A" name))
 (setf (ningle:route *web* "/hello/?:name?")
       (lambda ($params)
         (declare (ignorable $params))
         (apply (lambda (&key name &allow-other-keys)
                  (format nil "Hello, ~A" name))
                $params)))
 "defroute with no name")

(is-expand
 (defroute say-hello (*web* "/hello/?:name?") (&key name)
   (format nil "Hello, ~A" name))
 (prog1
     (defun say-hello (&key name &allow-other-keys)
       (format nil "Hello, ~A" name))
   (setf (ningle:route *web* "/hello/?:name?")
         (lambda ($params)
           (declare (ignorable $params))
           (apply #'say-hello $params))))
 "defroute with a name")

(finalize)
