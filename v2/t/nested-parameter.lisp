(in-package :cl-user)
(defpackage caveman2-nested-parameter-test
  (:use :cl
        :cl-test-more)
  (:import-from :caveman2.nested-parameter
                :parse-parameters
                :parse-key)
  (:import-from :trivial-types
                :property-list-p
                :association-list-p))
(in-package :caveman2-nested-parameter-test)

(plan 15)

(diag "parse-key")

(is (parse-key "name") '("name"))
(is (parse-key "my name") '("my name"))
(is (parse-key "person[]") '("person" ""))
(is (parse-key "person[][name]") '("person" "" "name"))
(is (parse-key "person[") '("person["))
(is (parse-key "person]") '("person]"))

(diag "parse-parameters")

(defun equal-content-p (a b)
  (when (equal a b)
    (return-from equal-content-p t))
  (cond
    ((and (property-list-p a)
          (property-list-p b))
     (every
      (lambda (a-key)
        (let ((a-val (getf a a-key)))
          (if (consp a-val)
              (if (listp b)
                  (equal-content-p a-val (getf b a-key))
                  nil)
              (equal a-val (getf b a-key)))))
      (remove-duplicates
       (loop for k in (append a b) by #'cddr
             collect k)
       :test #'equal)))
    ((and (association-list-p a)
          (association-list-p b))
     (loop for (a-key . a-val) in a
           for (b-key . b-val) in b
           unless (and (equal-content-p a-key b-key)
                       (equal-content-p a-val b-val))
             do (return-from equal-content-p nil))
     t)
    ((and (listp a)
          (listp b))
     (every (lambda (a-val b-val)
              (equal-content-p a-val b-val))
            a b))
    (T (equal a b))))

(defun is-params (params expected &optional desc)
  (let ((parsed (parse-parameters params)))
    (if (equal-content-p parsed expected)
        (ok t desc)
        (is parsed expected desc))))

(is (parse-parameters nil)
    nil
    "NIL")

(is-params '(("name" . "Eitaro") ("age" . 26))
           '(("name" . "Eitaro") ("age" . 26))
           "Not-nested case")

(is-params '(("friend_id[]" . 1) ("friend_id[]" . 2) ("age" . 26) ("me[name]" . "Eitaro"))
           '(("friend_id" . (1 2)) ("age" . 26) ("me" . (("name" . "Eitaro"))))
           "Normal case")

(is-params '(("me[name]" . "Eitaro") ("me[birthday][year]" . 1987) ("me[birthday][month]" . 13))
           '(("me" . (("name" . "Eitaro") ("birthday" . (("year" . 1987) ("month" . 13))))))
           "Nested case")

(is-params '(("Check[date]" . "2013/11/05")
             ("Check[place]" . "Walmart")
             ("submit" . "Save")
             ("Check[spendings][][name]" . "cabbage")
             ("Check[spendings][][amount]" . "2.4")
             ("Check[spendings][][unit]" . "1")
             ("Check[spendings][][price]" . "25")
             ("Check[spendings][][tags]" . "vegetables, food")
             ("Check[spendings][][name]" . "screwdriver")
             ("Check[spendings][][amount]" . "1")
             ("Check[spendings][][unit]" . "3")
             ("Check[spendings][][price]" . "10")
             ("Check[spendings][][tags]" . "tools, equipment"))
           '(("Check" . (("date" . "2013/11/05")
                         ("place" . "Walmart")
                         ("spendings" . ((("name" . "cabbage")
                                          ("amount" . "2.4")
                                          ("unit" . "1")
                                          ("price" . "25")
                                          ("tags" . "vegetables, food"))
                                         (("name" . "screwdriver")
                                          ("amount" . "1")
                                          ("unit" . "3")
                                          ("price" . "10")
                                          ("tags" . "tools, equipment"))))))
             ("submit" . "Save"))
           "Multiple records")

(is-params '(("name[" . "Eitaro") ("age" . 26))
           '(("name[" . "Eitaro") ("age" . 26))
           "Invalid key name")
(is-params '(("name]" . "Eitaro") ("age" . 26))
           '(("name]" . "Eitaro") ("age" . 26))
          "Invalid key name")

(is-params '(("my name[family]" . "Eitaro") ("age" . 26))
           '(("my name" . (("family" . "Eitaro") ("age" . 26))))
           "Key name contains a space")

(is-params '(("item [game] type" . "Hardware"))
           '(("item [game] type" . "Hardware")))

(finalize)
