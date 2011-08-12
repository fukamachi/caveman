(clack.util:namespace <% @var name %>.app
  (:use :cl)
  (:import-from :caveman.app
                :<app>))

(cl-annot:enable-annot-syntax)

@export
(defclass <<% @var name %>-app> (<app>) ())

@export
(defvar *app* (make-instance '<<% @var name %>-app>))
