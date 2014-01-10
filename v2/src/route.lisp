#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2.route
  (:use :cl
        :cl-annot)
  (:import-from :caveman2.app
                :find-package-app)
  (:import-from :caveman2.nested-parameter
                :parse-parameters)
  (:import-from :cl-annot.util
                :progn-form-last
                :progn-form-replace-last
                :definition-form-symbol
                :definition-form-type)
  (:export :defroute
           :route))
(in-package :caveman2.route)

(defun add-app-if-omitted (routing-rule)
  (let ((route-args (gensym "ROUTE-ARGS")))
    `(let ((,route-args ,(if (listp routing-rule)
                             `(list ,@routing-rule)
                             `(list ,routing-rule))))
       (if (stringp (car ,route-args))
           (cons (find-package-app ,*package*) ,route-args)
           ,route-args))))

(defun make-lambda-list (lambda-list)
  (if (and lambda-list
           (member 'cl:&key lambda-list :test #'eq)
           (not (eq (car (last lambda-list)) 'cl:&allow-other-keys)))
      (append lambda-list '(cl:&allow-other-keys))
      lambda-list))

(defmacro defroute (&rest args)
  (let ((params (gensym "PARAMS")))
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
                         `(apply (function ,name) (parse-parameters ,params))
                         `(funcall (function ,name))))))))
      (list
       (destructuring-bind (routing-rule lambda-list &rest body) args
         `(setf (apply #'ningle:route
                       ,(add-app-if-omitted routing-rule))
                (lambda (,params)
                  (declare (ignorable ,params))
                  ,(if lambda-list
                       `(apply (lambda ,(make-lambda-list lambda-list) ,@body)
                               (parse-parameters ,params))
                       `(progn ,@body))))))
      (T `(defroute (,(car args)) ,@(cdr args))))))

(defannotation route (method routing-rule form)
    (:arity 3)
  (let* ((params (gensym "PARAMS"))
         (last-form (progn-form-last form))
         (type (definition-form-type last-form))
         (symbol (definition-form-symbol last-form))
         (lambda-list (third last-form)))
    (ecase type
      (cl:lambda `(setf (apply #'ningle:route
                    (append
                     ,(add-app-if-omitted routing-rule)
                     (list :method ',method)))
              (lambda (,params)
                (declare (ignorable ,params))
                ,(if lambda-list
                     `(apply ,form (parse-parameters ,params))
                     `(funcall ,form)))))
      (cl:defun `(progn
                   (setf (apply #'ningle:route
                                (append
                                 ,(add-app-if-omitted routing-rule)
                                 (list :method ',method
                                       :identifier ',symbol)))
                         (lambda (,params)
                           (declare (ignorable ,params))
                           ,(if lambda-list
                                `(apply (function ,symbol) (parse-parameters ,params))
                                `(funcall (function ,symbol)))))
                   ,(progn-form-replace-last
                     (list* (first last-form) (second last-form)
                            (make-lambda-list lambda-list)
                            (cdddr last-form))
                     form))))))
