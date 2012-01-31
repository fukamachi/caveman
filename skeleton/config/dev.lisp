`(:static-path #p"static/"
  :log-path #p"log/"
  :template-path #p"tmpl/"
  :application-root ,(asdf:component-pathname
                      (asdf:find-system :<% @var name %>))
  :server :hunchentoot
  :port 5000)
