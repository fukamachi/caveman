#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.request
  (:use :cl
        :clack.request)
  (:shadow :<request>)
  (:export :request-method
           :script-name
           :path-info
           :server-name
           :server-port
           :server-protocol
           :request-uri
           :uri-scheme
           :remote-addr
           :remote-port
           :query-string
           :raw-body
           :content-length
           :content-type
           :clack-handler

           :referer
           :user-agent
           :uploads

           :securep
           :cookies
           :body-parameter
           :query-parameter
           :parameter))

(cl-annot:enable-annot-syntax)

@export
(defclass <request> (clack.request:<request>) ())

@export
(defun make-request (req)
  (apply #'make-instance '<request> :allow-other-keys t req))
