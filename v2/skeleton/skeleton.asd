(defsystem "<% @var name %>"
  :version "0.1.0"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "<% @var description %>"
  :in-order-to ((test-op (test-op "<% @var name %>-tests"))))

(defsystem "<% @var name %>-tests"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on ("<% @var name %>"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "<% @var name %>"))))
  :description "Test system for <% @var name %>"
  :perform (test-op (op c) (symbol-call :rove :run c)))
