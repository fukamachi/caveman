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
                :*context*
                :*request*
                :*response*
                :*session*
                :context
                :with-context-variables)
  (:import-from :caveman.view
                :render)
  (:export :url
           :link-to
           :*context*
           :*request*
           :*response*
           :*session*
           :context
           :with-context-variables
           :render))

(use-syntax annot-syntax)

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
