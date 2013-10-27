;; -*- mode: common-lisp -*-

(ql:quickload '(:<% @var name %> :split-sequence))

(import 'split-sequence:split-sequence)

#+sbcl(setf sb-impl::*default-external-format* :utf-8)
#+sbcl(setf sb-alien::*default-c-string-external-format* :utf-8)

(defun start (&key (port 8080)
                   (server :fcgi)
                   (debug nil))
  (flet ((start (&rest args)
           (apply #'<% @var name %>:start
                  :debug debug :server server args)))
    (let ((server-starter-port (asdf::getenv "SERVER_STARTER_PORT")))
      (if server-starter-port
          (destructuring-bind (port fd)
              (split-sequence #\=
                              (car (split-sequence #\; server-starter-port :count 1)))
            (start :port (parse-integer port)
                   :fd (parse-integer fd)))
          (start :port port)))))
