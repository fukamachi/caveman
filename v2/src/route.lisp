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
  (if (or (and (listp routing-rule)
               (stringp (car routing-rule)))
          (stringp routing-rule))
      `(cons (find-package-app ,*package*) ,(if (listp routing-rule)
                                                `(list ,@routing-rule)
                                                `(list ,routing-rule)))
      (if (listp routing-rule)
          `(list ,@routing-rule)
          `(list ,routing-rule))))

;; Add &allow-other-keys if &key exists.
(defun make-lambda-list (lambda-list)
  (if (and lambda-list
           (member 'cl:&key lambda-list :test #'eq)
           (not (eq (car (last lambda-list)) 'cl:&allow-other-keys)))
      (append lambda-list '(cl:&allow-other-keys))
      lambda-list))

(defun parse-key-arguments (lambda-list)
  (loop for (arg . rest-args) on lambda-list
        if (eq arg 'cl:&key)
          do (return
               (loop for arg in rest-args
                     until (and (symbolp arg)
                                (eq (symbol-package arg) (find-package :common-lisp))
                                (char= (aref (symbol-name arg) 0) #\&))
                     collect arg))))

(defun params-form (params-symb lambda-list)
  (let ((pair (gensym "PAIR")))
    `(nconc ,@(loop for arg in (parse-key-arguments lambda-list)
                    collect (destructuring-bind (arg &optional default specified)
                                (if (consp arg) arg (list arg))
                              (declare (ignore default specified))
                              `(let ((,pair (assoc ,(if (or (string= arg :captures)
                                                            (string= arg :splat))
                                                        (intern (symbol-name arg) :keyword)
                                                        (symbol-name arg))
                                                   ,params-symb
                                                   :test #'string=)))
                                 (if ,pair
                                     (list ,(intern (symbol-name arg) :keyword) (cdr ,pair))
                                     nil)))))))

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
              ,(multiple-value-bind (body declarations documentation)
                   (alexandria:parse-body body :documentation t)
                 `(defun ,name (,params)
                    (declare (ignorable ,params))
                    ,(or documentation (format nil "Handler for ~a" routing-rule))
                    ,@(if lambda-list
                          `((destructuring-bind ,(make-lambda-list lambda-list)
                                ,(if (need-parsed-parameters lambda-list)
                                  `(append (list
                                            ,(intern *parsed-parameters-symbol-name* :keyword)
                                            (parse-parameters ,params))
                                           ,(params-form params lambda-list))
                                  (params-form params lambda-list))
                              ,@(if declarations (list declarations) nil)
                              ,@body))
                         body)))
            (setf (apply #'ningle:route
                         (append
                          ,(add-app-if-omitted routing-rule)
                          (list :identifier ',name)))
                  (function ,name)))))
      (list `(defroute ,(gensym "BODY") ,@args))
      (T `(defroute (,(car args)) ,@(cdr args))))))

(defun canonicalize-method (method)
  (etypecase method
    (list (mapcar #'canonicalize-method method))
    (keyword method)
    (symbol (intern (symbol-name method) :keyword))))

(defannotation route (method routing-rule form)
    (:arity 3)
  (let* ((params (gensym "PARAMS"))
         (last-form (progn-form-last form))
         (type (definition-form-type last-form))
         (symbol (definition-form-symbol last-form))
         lambda-list)
    (ecase type
      (cl:lambda
          (setf lambda-list (second last-form))
          `(setf (apply #'ningle:route
                  (append
                   ,(add-app-if-omitted routing-rule)
                   (list :method ',(canonicalize-method method))))
            (lambda (,params)
              (declare (ignorable ,params))
              ,(if lambda-list
                   `(apply ,form ,(if (need-parsed-parameters lambda-list)
                                      `(append (list
                                                ,(intern *parsed-parameters-symbol-name* :keyword)
                                                (parse-parameters ,params))
                                               ,(params-form params lambda-list))
                                      (params-form params lambda-list)))
                   `(funcall ,form)))))
      (cl:defun
          (setf lambda-list (third last-form))
          `(progn
             (setf (apply #'ningle:route
                          (append
                           ,(add-app-if-omitted routing-rule)
                           (list :method ',(canonicalize-method method)
                                 :identifier ',symbol)))
                   (lambda (,params)
                     (declare (ignorable ,params))
                     ,(if lambda-list
                          `(apply (function ,symbol)
                                  ,(if (need-parsed-parameters lambda-list)
                                       `(append (list
                                                 ,(intern *parsed-parameters-symbol-name* :keyword)
                                                 (parse-parameters ,params))
                                                ,(params-form params lambda-list))
                                       (params-form params lambda-list)))
                          `(funcall (function ,symbol)))))
             ,(progn-form-replace-last
               (list* (first last-form) (second last-form)
                      (make-lambda-list lambda-list)
                      (cdddr last-form))
               form))))))
