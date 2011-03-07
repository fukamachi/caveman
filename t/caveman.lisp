(clack.util:namespace caveman-test
  (:use :cl
        :caveman
        :cl-test-more
        :drakma
        :myapp))

(plan 2)

(is (http-request "http://localhost:8080/")
    "Hello, Caveman!"
    "index")

(is (http-request "http://localhost:8080/not-found-hoge")
    nil
    "not found")

(finalize)
