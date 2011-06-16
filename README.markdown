# Caveman - A micro web framework for Common Lisp

Caveman is a micro web framework for Common Lisp, based on [Clack](https://github.com/fukamachi/clack).

## Usage

    @url GET "/hi"
    (defun say-hi (params)
      "Hello, World!")

## Installation

Caveman is available on [Quicklisp](https://www.quicklisp.org/beta/).

    (ql:quickload :clack)

## Getting started

    (caveman.skeleton:generate :myapp)

Then a project skeleton is generated to current directory.

    (ql:quickload :myapp)
    (myapp:start)

Now you can access to http://localhost:8080/ and then Caveman may show you "Hello, Caveman!".

### Route

Caveman provides an useful annotation "@url" to define a controller (You don't already know the meaning of "annotation"? Check [cl-annot](https://github.com/m2ym/cl-annot) out). It has same rules to [Clack.App.Route](http://clacklisp.org/doc/clack.app.route.html), it is an HTTP method paired with URL-matching pattern.

    @url GET "/"
    (defun index (params) ...)
    
    @url POST "/"
    (defun index (params) ...)
    
    @url PUT "/"
    (defun index (params) ...)
    
    @url DELETE "/"
    (defun index (params) ...)
    
    @url OPTIONS "/"
    (defun index (params) ...)

Route pattern may contain "keyword" to put the value into the argument.

    @url GET "/hello/:name"
    (defun hello (params)
      (format nil "Hello, ~A" (getf params :name)))

The above controller will be invoked when you access to "/hello/Eitarow" and "/hello/Tomohiro", and then `(getf params :name)` will be "Eitarow" and "Tomohiro".

Route patterns may also contain "wildcard" parameters. They are accessible to run `(getf params :splat)`.

    @url GET "/say/*/to/*"
    (defun say (params)
      ; matches /say/hello/to/world
      (getf params :splat) ;=> ("hello" "world")
      )
    
    @url GET "/download/*.*"
    (defun download ()
      ; matches /download/path/to/file.xml
      (getf params :splat) ;=> ("path/to/file" "xml")
      )

### Return Value

You can return following format as the result in actions.

* String
* Pathname
* Clack's response list (containing Status, Headers and Body)

### View

Caveman adopt CL-EMB as the default template engine. A package, named `myapp.view.emb`, will be generated in your project which has one function `render`. It is simply execute `emb:execute-emb` and return the result as a string.

Of course, you can use other template engines, such as "cl-markup".

### Configuration

Caveman uses ".lisp" file as configuration file in `#p"config/"` directory. When a project is just generated, you might be able to find `dev.lisp` in it. It will be used when "start" the project application with "dev" mode.

    ;; config/dev.lisp
    `(:static-path #p"public/"
      :log-path #p"log/"
      :template-path #p"src/tmpl/"
      :application-root ,(asdf:component-pathname
                          (asdf:find-system :myapp))
      :server :hunchentoot
      :port 8080
      :database-type :sqlite3
      :database-connection-spec (,(namestring
                                   (asdf:system-relative-pathname
                                    :myapp
                                    "sqlite3.db"))))

Obviously, this is just a plist. You can use following keys in there.

* `:application-root` (Pathname): Pathname of the application root.
* `:static-path` (Pathname): Relative pathname of a static files directory from the root.
* `:log-path` (Pathname): Relative pathname of a log files directory from the root.
* `:template-path` (Pathname): Relative pathname of a template directory from the root.
* `:port` (Integer): Server port.
* `:server` (Keyword): Clack.Handler's server type. (ex. `:hunchentoot`)

And following stuffs will be used by Clack.Middleware.Clsql  for integrating CLSQL.

* `:database-type` (Keyword)
* `:database-connection-spec` (List)

You can access to the configuration plist anywhere, by using `myapp:config`.

    (myapp:config)
    ;;=> (:static-path #p"public/" :template-path ...)
    (myapp:config :server)
    ;;=> :hunchentoot

### Helper

* `context`
* `with-context-variables`
* `config`
* `app-path`
* `url-for`
* `redirect-to`
* `forward-to`
* `current-uri`

## More practical

### Extend the Context

### Database

## Dependency

* [Clack](https://github.com/fukamachi/clack)
* [cl-annot](https://github.com/arielnetworks/cl-annot)
* [cl-syntax](https://github.com/m2ym/cl-syntax)
* CL-PPCRE
* CL-FAD
* CLSQL
* CL-EMB
* LOCAL-TIME

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Contributors

* Tomohiro Matsuyama (tomo@cx4a.org)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## License

Licensed under the LLGPL License.
