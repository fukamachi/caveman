(clack.util:namespace caveman-test.init
  (:use :cl
        :cl-test-more))

(plan 0)

(defvar *project-root*
    (asdf:system-relative-pathname :caveman "t/tmp/"))

(defvar *myapp-root*
    (merge-pathnames "myapp/" *project-root*))

(when (cl-fad:file-exists-p *myapp-root*)
  (cl-fad:delete-directory-and-files *myapp-root*))
(ensure-directories-exist *project-root*)

(caveman.skeleton:generate *myapp-root*)

(diag "loading myapp...")
(load (merge-pathnames "myapp.asd" *myapp-root*))
(asdf:load-system (asdf:find-system :myapp))

(diag "myapp start")
(funcall (intern "START" :myapp) :debug t)

(finalize)
