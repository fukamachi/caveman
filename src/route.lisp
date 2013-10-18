#|
  This file is a part of caveman project.
  Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman.route
  (:use :cl)
  (:import-from :ningle
                :next-route)
  (:export :defroute
           :next-route))
(in-package :caveman.route)

(defmacro defroute (route-args args &body body)
  (let ((params (gensym "PARAMS")))
    `(setf (ningle:route ,@route-args)
           ,(if args
                `#'(lambda (,params)
                     (apply
                      #'(lambda ,(if (and (member 'cl:&key args :test #'eq)
                                     (not (eq (car (last args)) 'cl:&allow-other-keys)))
                                (append args '(cl:&allow-other-keys))
                                args)
                          ,@body)
                      ,params))
                `#'(lambda (,params)
                     (declare (ignore ,params))
                     ,@body)))))
