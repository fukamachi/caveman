(in-package :cl-user)
(defpackage <% @var name %>.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*))
(in-package :<% @var name %>.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :<% @var name %>))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defconfig |default|
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))
