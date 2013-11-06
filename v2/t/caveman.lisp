#|
  This file is a part of caveman project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage caveman2-test
  (:use :cl
        :caveman2
        :cl-test-more
        :usocket
        :cl-fad))
(in-package :caveman2-test)

(plan 4)

(defun port-available-p (port)
  (handler-case (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))

(defun find-port-not-in-use (&key (from-port 50000) (to-port 60000))
  (loop for port from (+ from-port (random (- to-port from-port))) upto to-port
        if (port-available-p port)
          return port))

(defparameter *app-name*
  (loop for name = (symbol-name (gensym "myapp"))
        while (asdf:find-system name nil)
        finally (return name)))

(defparameter *tmp-root*
  (asdf:system-relative-pathname :caveman2 "v2/t/tmp/"))

(defparameter *project-root*
  (merge-pathnames (format nil "~A/" *app-name*) *tmp-root*))

(defparameter *cl-emb-intern-package-name*
  (intern (format nil "CL-EMB-INTERN-~A" *app-name*)
          :keyword))

(let ((emb:*function-package* (eval
                               `(defpackage ,*cl-emb-intern-package-name*
                                  (:use :cl)))))

  (when (cl-fad:file-exists-p *tmp-root*)
    (cl-fad:delete-directory-and-files *tmp-root*))
  (ensure-directories-exist *tmp-root*)

  (caveman2:make-project *project-root*)
  (load (merge-pathnames (format nil "~A.asd" *app-name*) *project-root*))
  (asdf:load-system *app-name*)

  (let* ((port (find-port-not-in-use)))
    (ok (funcall (intern #.(string :start) (string-upcase *app-name*)) :port port))
    (multiple-value-bind (body status)
        (drakma:http-request (format nil "http://127.0.0.1:~D"
                                     port))
      (is status 200)
      (like body "Welcome to Caveman2"))
    (ok (funcall (intern #.(string :stop) (string-upcase *app-name*))))))

(delete-package *cl-emb-intern-package-name*)

(finalize)

(asdf:clear-system *app-name*)
