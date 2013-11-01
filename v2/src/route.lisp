#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2.route
  (:use :cl)
  (:export :defroute))
(in-package :caveman2.route)

(defmacro defroute (&rest args)
  (let ((params (gensym "PARAMS")))
    (flet ((make-lambda-list (lambda-list)
             (if (and lambda-list
                      (member 'cl:&key lambda-list :test #'eq)
                      (not (eq (car (last lambda-list)) 'cl:&allow-other-keys)))
                 (append lambda-list '(cl:&allow-other-keys))
                 lambda-list)))
      (etypecase (car args)
        (symbol
         (destructuring-bind (name route-args lambda-list &rest body) args
           `(prog1
                (defun ,name ,(make-lambda-list lambda-list)
                  ,@body)
              (setf (ningle:route ,@route-args :identifier ',name)
                    (lambda (,params)
                      (declare (ignorable ,params))
                      ,(if lambda-list
                           `(apply (function ,name) ,params)
                           `(funcall (function ,name))))))))
        (list
         (destructuring-bind (route-args lambda-list &rest body) args
           `(setf (ningle:route ,@route-args)
                  (lambda (,params)
                    (declare (ignorable ,params))
                    ,(if lambda-list
                         `(apply (lambda ,(make-lambda-list lambda-list) ,@body)
                                 ,params)
                         `(progn ,@body))))))))))
