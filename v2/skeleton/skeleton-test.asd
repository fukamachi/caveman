(defsystem "<% @var name %>-test"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on ("<% @var name %>"
               "prove")
  :components ((:module "t"
                :components
                ((:file "<% @var name %>")))))
