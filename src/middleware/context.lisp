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
                :make-context
                :request*
                :response*)
  (:import-from :caveman.response
                :make-response
                :body
                :finalize))

(cl-annot:enable-annot-syntax)

@export
(defclass <caveman-middleware-context> (<middleware>)
     ((context :initarg :context :initform (intern "*CONTEXT*" *package*))))

(defmethod call ((this <caveman-middleware-context>) req)
  (let ((context (make-context req)))
    (eval `(let* ((,(slot-value this 'context) ,context)
                  (result (call-next ,this ,(request* context))))
             (if (listp result)
                 result
                 (progn (setf (body ,(response* context)) result)
                        (finalize ,(response* context))))))))
