#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2.route
  (:use :cl)
  (:import-from :caveman2.app
                :find-package-app)
  (:export :defroute))
(in-package :caveman2.route)

(defun add-app-if-omitted (routing-rule)
  (let ((route-args (gensym "ROUTE-ARGS")))
    `(let ((,route-args ,(if (listp routing-rule)
                             `(list ,@routing-rule)
                             `(list ,routing-rule))))
       (if (stringp (car ,route-args))
           (cons (find-package-app ,*package*) ,route-args)
           ,route-args))))

(defmacro defroute (&rest args)
  (let ((params (gensym "PARAMS"))
        (route-args (gensym "ROUTE-ARGS")))
    (flet ((make-lambda-list (lambda-list)
             (if (and lambda-list
                      (member 'cl:&key lambda-list :test #'eq)
                      (not (eq (car (last lambda-list)) 'cl:&allow-other-keys)))
                 (append lambda-list '(cl:&allow-other-keys))
                 lambda-list)))
      (typecase (car args)
        (symbol
         (destructuring-bind (name routing-rule lambda-list &rest body) args
           `(prog1
                (defun ,name ,(make-lambda-list lambda-list)
                  ,@body)
              (setf (apply #'ningle:route
                           (append
                            ,(add-app-if-omitted routing-rule)
                            (list :identifier ',name)))
                    (lambda (,params)
                      (declare (ignorable ,params))
                      ,(if lambda-list
                           `(apply (function ,name) ,params)
                           `(funcall (function ,name))))))))
        (list
         (destructuring-bind (routing-rule lambda-list &rest body) args
           `(setf (apply #'ningle:route
                         ,(add-app-if-omitted routing-rule))
                  (lambda (,params)
                    (declare (ignorable ,params))
                    ,(if lambda-list
                         `(apply (lambda ,(make-lambda-list lambda-list) ,@body)
                                 ,params)
                         `(progn ,@body))))))
        (T `(defroute (,(car args)) ,@(cdr args)))))))
