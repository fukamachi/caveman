`(:static-path #p"public/"
  :log-path #p"log/"
  :template-path #p"tmpl/"
  :application-root ,(asdf:component-pathname
                      (asdf:find-system :<% @var name %>))
  :server :hunchentoot
  :port 8080
  :database-type :sqlite3
  :database-connection-spec (,(namestring
                               (asdf:system-relative-pathname
                                :<% @var name %>
                                "sqlite3.db"))))
