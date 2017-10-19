(defsystem "<% @var name %>-test"
  :defsystem-depends-on ("prove-asdf")
  :author "<% @var author %>"
  :license "<% @var license %>"
  :depends-on ("<% @var name %>"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "<% @var name %>"))))
  :description "Test system for <% @var name %>"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
