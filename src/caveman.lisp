#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.static
        :clack.middleware.clsql)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :cl-fad
                :file-exists-p)
  (:import-from :clack.request
                :make-request)
  (:import-from :caveman.database
                :database-setup)
  (:import-from :cl-annot.doc
                :doc))

(cl-annot:enable-annot-syntax)

@export
(defclass <app> (<component>)
     ((config :initarg :config :initform nil
              :accessor config)
      (routing-rules :initarg routing-rules :initform nil
                     :accessor routing-rules)))

@export
(defmethod setup ((this <app>))
  (let ((config (config this)))
    (when (getf config :config-file)
      (let ((config-file (merge-pathnames (getf config :config-file)
                                        (getf config :application-root))))
        (when (file-exists-p config-file)
          (load config-file))))
    (database-setup (getf config :database-type)
                    (getf config :database-connection-spec))))

@export
(defmethod start ((this <app>)
                  &key port server debug lazy)
  (setup this)
  (setf *builder-lazy-p* lazy)
  (clackup (builder
            (<clack-middleware-static>
             :path "/public/"
             :root (merge-pathnames (getf (config this) :static-path)
                                    (getf (config this) :application-root)))
            (<clack-middleware-clsql>
             :database-type (getf (config this) :database-type)
             :connection-spec (getf (config this) :database-connection-spec))
            this)
           :port (or port (getf (config this) :port))
           :debug debug
           :server (or server (getf (config this) :server))))

@export
(defmethod call ((this <app>) req)
  "Dispatch HTTP request to each actions.
This returns a Clack Application."
  (let ((method (getf req :request-method))
        (path-info (getf req :path-info)))
    (loop for rule in (reverse (routing-rules this))
          for (meth (re vars) fn) = (cdr rule)
          if (string= meth method)
            do (multiple-value-bind (matchp res)
                   (scan-to-strings re path-info)
                 (when matchp
                   (let ((req (make-request req))
                         (params
                          (loop for key in vars
                                for val in (coerce res 'list)
                                append (list
                                         (intern (symbol-name key) :keyword)
                                         val))))
                     (setf (slot-value req 'clack.request::query-parameters)
                           (append
                            params
                            (slot-value req 'clack.request::query-parameters)))
                     (return (call fn req)))))
          finally (return '(404 nil nil)))))

@export
(defmethod add-route ((this <app>) routing-rule)
  (setf (routing-rules this)
        (delete (car routing-rule)
                (routing-rules this)
                :key #'car))
  (push routing-rule
        (routing-rules this)))

(defvar *copy-file-hook* nil)

(defun copy-directory (source-dir target-dir)
  (ensure-directories-exist target-dir)
  #+allegro
  (excl:copy-directory source-dir target-dir :quiet t)
  #-allegro
  (loop for file in (cl-fad:list-directory source-dir)
        if (cl-fad:directory-pathname-p file)
          do (copy-directory
                  file
                  (concatenate 'string
                               (directory-namestring target-dir)
                               (car (last (pathname-directory file))) "/"))
        else
          do (copy-file-to-dir file target-dir))
  t)

(defun copy-file-to-dir (source-path target-dir)
  (let ((target-path (make-pathname
                      :directory (directory-namestring target-dir)
                      :name (pathname-name source-path)
                      :type (pathname-type source-path))))
    (cl-fad:copy-file source-path target-path)
    (when *copy-file-hook*
      (funcall *copy-file-hook* target-path))))

(defun slurp-file (path)
  (with-open-file (stream path :direction :input)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

@export
(defun make-app (name &key (path (truename #p"./")))
  (setf name (string-downcase name))
  (let* ((root
          (merge-pathnames (concatenate 'string name "/") path))
         (*copy-file-hook*
          #'(lambda (path)
              (let ((content (slurp-file path)))
                (with-open-file (stream path :direction :output :if-exists :supersede)
                  (write-sequence
                   (ppcre:regex-replace-all "\\${.+?}" content
                    #'(lambda (string &rest args)
                        @ignore args
                        (cond
                          ((string= string "${application-root}")
                           (prin1-to-string root))
                          ((string= string "${application-name}")
                           name)
                          (t string)))
                    :simple-calls t)
                   stream)))
              (when (string= (pathname-name path) "skelton")
                (rename-file path
                             (concatenate 'string
                                          name "." (pathname-type path))))
              )))
    (copy-directory
     #.(merge-pathnames
        #p"skelton/"
        (asdf:component-pathname (asdf:find-system :caveman)))
     root)))
