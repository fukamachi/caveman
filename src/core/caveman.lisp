#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman
  (:use :cl)
  (:import-from :cl-syntax
                :use-syntax)
  (:import-from :cl-syntax-annot
                :annot-syntax)
  (:import-from :caveman.route
                :url
                :link-to)
  (:import-from :caveman.context
                :*app*
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :with-context-variables)
  (:export :url
           :link-to
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :with-context-variables))

(use-syntax annot-syntax)

@export
(defun config (&optional key)
  (let ((conf (caveman.app:config *app*)))
    (if key (getf conf key) conf)))

(doc:start)

@doc:NAME "
Caveman - main package.
"

@doc:DESCRIPTION "
This package is main package just for convenient. Your Caveman Application may use this package.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
