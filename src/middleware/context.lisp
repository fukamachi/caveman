#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.middleware.context
  (:use :cl
        :clack)
  (:import-from :caveman.context
                :<context>)
  (:import-from :caveman.request
                :make-request)
  (:import-from :caveman.response
                :make-response))

(cl-annot:enable-annot-syntax)

@export
(defclass <caveman-middleware-context> (<middleware>)
     ((context :initform (intern "*CONTEXT*" *package*))))

(defmethod call ((this <caveman-middleware-context>) req)
  (let* ((request (make-request req))
         (response (make-response 200 ()))
         (ctx (make-instance '<context> :request request :response response)))
    (eval `(let ((,(slot-value this 'context) ctx))
             (call-next this request)))))
