(in-package :cl-user)
(defpackage caveman-test.init
  (:use :cl
        :cl-test-more))
(in-package :caveman-test.init)

(plan 0)

;; Utilities {{

(defun port-available-p (port)
  (handler-case (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))

(defun find-port-not-in-use (&key (from-port 50000) (to-port 60000))
  (loop for port from (+ from-port (random (- to-port from-port))) upto to-port
        if (port-available-p port)
          return port))

;; }} Utilities

(defvar *project-root*
    (asdf:system-relative-pathname :caveman "v1/t/tmp/"))

(defvar *myapp-root*
    (merge-pathnames "myapp/" *project-root*))

(defvar *myapp-port* (find-port-not-in-use))
(defvar *myapp-url*
    (format nil "http://127.0.0.1:~D" *myapp-port*))

(when (cl-fad:file-exists-p *myapp-root*)
  (cl-fad:delete-directory-and-files *myapp-root*))
(ensure-directories-exist *project-root*)

(caveman.skeleton:generate *myapp-root*)

(diag "loading myapp...")
(load (merge-pathnames "myapp.asd" *myapp-root*))
(asdf:load-system (asdf:find-system :myapp))

(diag "myapp start")
(funcall (intern "START" :myapp) :port *myapp-port* :debug t)

(finalize)
