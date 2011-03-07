(clack.util:namespace caveman-test
  (:use :cl
        :caveman
        :cl-test-more
        :drakma))

(plan 5)

(defvar *project-root*
    (asdf:system-relative-pathname :caveman "t/tmp/"))

(defvar *myapp-root*
    (merge-pathnames "myapp/" *project-root*))

(when (cl-fad:file-exists-p *myapp-root*)
  (cl-fad:delete-directory-and-files *myapp-root*))
(ensure-directories-exist *project-root*)

(ok (caveman:make-app :myapp :path *project-root*)
    "generate skelton")

(load (merge-pathnames "myapp.asd" *myapp-root*))
(ok (asdf:find-system :myapp) "asd file is there")

(diag "loading myapp...")
(asdf:load-system (asdf:find-system :myapp))

(diag "myapp start")
(funcall (intern "START" :myapp) :debug t)

(is (http-request "http://localhost:8080/")
    "Hello, Caveman!"
    "index")

(is (http-request "http://localhost:8080/not-found-hoge")
    nil
    "not found")

(diag "myapp stop")
(funcall (intern "STOP" :myapp))

(is (symbol-value (intern "*ACCEPTOR*" :myapp))
    nil
    "finalized")

(cl-fad:delete-directory-and-files *myapp-root*)
(finalize)
