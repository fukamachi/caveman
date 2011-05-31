`(:static-path #p"src/public/"
  :template-path #p"src/tmpl/"
  :application-root ,(asdf:component-pathname
                      (asdf:find-system :${application-name}))
  :server :hunchentoot
  :port 8080
  :database-type :sqlite3
  :database-connection-spec (,(namestring
                               (asdf:system-relative-pathname
                                :${application-name}
                                "sqlite3.db"))))
