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
           :route
           :*parsed-parameters-symbol-name*))
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

(defparameter *parsed-parameters-symbol-name* #.(string :_parsed))

(defun need-parsed-parameters (lambda-list)
  (member-if (lambda (p)
               (and (symbolp p)
                    (string= *parsed-parameters-symbol-name* p)))
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
                         `(apply (function ,name) ,(if (need-parsed-parameters lambda-list)
                                                       `(append (list
                                                                 ,(intern *parsed-parameters-symbol-name* :keyword)
                                                                 (parse-parameters ,params))
                                                                ,params)
                                                       params))
                         `(funcall (function ,name))))))))
      (list
       (destructuring-bind (routing-rule lambda-list &rest body) args
         `(setf (apply #'ningle:route
                       ,(add-app-if-omitted routing-rule))
                (lambda (,params)
                  (declare (ignorable ,params))
                  ,(if lambda-list
                       `(apply (lambda ,(make-lambda-list lambda-list) ,@body)
                               ,(if (need-parsed-parameters lambda-list)
                                    `(append (list
                                              ,(intern *parsed-parameters-symbol-name* :keyword)
                                              (parse-parameters ,params))
                                             ,params)
                                    params))
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
                     `(apply ,form ,(if (need-parsed-parameters lambda-list)
                                        `(append (list
                                                  ,(intern *parsed-parameters-symbol-name* :keyword)
                                                  (parse-parameters ,params))
                                                 ,params)
                                        params))
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
                                `(apply (function ,symbol)
                                        ,(if (need-parsed-parameters lambda-list)
                                             `(append (list
                                                       ,(intern *parsed-parameters-symbol-name* :keyword)
                                                       (parse-parameters ,params))
                                                      ,params)
                                             params))
                                `(funcall (function ,symbol)))))
                   ,(progn-form-replace-last
                     (list* (first last-form) (second last-form)
                            (make-lambda-list lambda-list)
                            (cdddr last-form))
                     form))))))
