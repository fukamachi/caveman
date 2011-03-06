#|
  This file is a part of Caveman package.
  URL: http://github.com/fukamachi/caveman
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Caveman is freely distributable under the LLGPL License.
|#

(clack.util:namespace caveman.skelton
  (:use :cl)
  (:import-from :cl-fad
                :list-directory
                :copy-file)
  (:import-from :cl-ppcre
                :regex-replace-all))

(cl-annot:enable-annot-syntax)

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
     #.(asdf:system-relative-pathname
        :caveman
        #p"skelton/")
     root)))
