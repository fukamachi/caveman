`(:static-path #p"public/"
  :template-path #p"tmpl/"
  :server :hunchentoot
  :port 8080
  :database-type :sqlite3
  :database-connection-spec (,(namestring
                               (asdf:system-relative-pathname
                                :${application-name}
                                "sqlite3.db"))))
