# Caveman - A micro web framework for Common Lisp

Caveman is a micro web framework for Common Lisp, based on [Clack](http://clacklisp.org).

* [https://github.com/fukamachi/caveman](https://github.com/fukamachi/caveman)

## Usage

```common-lisp
@url GET "/hi"
(defun say-hi (params)
  "Hello, World!")
```

## What's Caveman

Caveman is a micro web framework on [Clack](http://clacklisp.org).

Why we should use "Framework" or something even if we already have Clack. You know Clack provides a very extensible environment for web application. We can build applications from isolated parts of Clack like kneading dough clay.

But Clack isn't a real framework. If you say that Clack is a collection of cells, Caveman is a newborn baby. Caveman provides a minimum set for building web applications. You can decorate the baby as you like, of course, and also you can replace any parts in it.

Caveman has following features:

* Thin
* Extensible
* Easy to understand

## Installation

Caveman is available on [Quicklisp](https://www.quicklisp.org/beta/).

```common-lisp
(ql:quickload :caveman)
```

## Getting started

First, you have to generate a skeleton project.

```common-lisp
(caveman.skeleton:generate #p"lib/myapp/")
```

Then a project skeleton is generated at `lib/myapp/`. The new project can be loaded and runs on this state.

```common-lisp
(ql:quickload :myapp)
(myapp:start)
```

Now you can access to http://localhost:5000/ and then Caveman may show you "Hello, Caveman!".

### Route

Caveman provides an useful annotation "@url" to define a controller (You don't already know the meaning of "annotation"? Check [cl-annot](https://github.com/m2ym/cl-annot) out). It has same rules to [Clack.App.Route](http://clacklisp.org/doc/clack.app.route.html), it is an HTTP method paired with URL-matching pattern.

```common-lisp
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
```

Route pattern may contain "keyword" to put the value into the argument.

```common-lisp
@url GET "/hello/:name"
(defun hello (params)
  (format nil "Hello, ~A" (getf params :name)))
```

The above controller will be invoked when you access to "/hello/Eitarow" and "/hello/Tomohiro", and then `(getf params :name)` will be "Eitarow" and "Tomohiro".

Route patterns may also contain "wildcard" parameters. They are accessible to run `(getf params :splat)`.

```common-lisp
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
```

### Passing

Normally, routes are matched in the order they are defined. Only the first route matched is invoked and rest of them just will be ignored. But, a route can punt processing to the next matching route using `next-route`.

```common-lisp
@url GET "/guess/:who"
(defun guess-me (params)
  (unless (string= (getf params :who) "Eitarow")
    (next-route))
  "You got me!")

@url GET "/guess/*"
(defun guess-anyone (params)
  "You missed!")
```

### Return Value

You can return following format as the result in actions.

* String
* Pathname
* Clack's response list (containing Status, Headers and Body)

### View

Caveman adopt CL-EMB as the default template engine. A package, named `myapp.view.emb`, will be generated in your project which has one function `render`. It is simply execute `emb:execute-emb` and return the result as a string.

Of course, you can use other template engines, such as "cl-markup".

Example of using cl-markup for rendering
```common-lisp
@url GET "/mypage"
(defun func-test(params)
  (setf params (nconc 
  	       	      (list :MUSIC)
		      (list (list
		      	    '(:artist "DJ X" :song "Track 1")
		      	    '(:artist "DJ Y" :song "Track 8")
			    '(:artist "DJ Z" :song "Let's rock!")))))
  (render "track-list.tpl" params))
```

In the file "track-list.tpl" which is in the templates folder
```common-lisp
<!-- track-list.tpl -->
<% @loop music %>
   <% @var artist %> tonight with <% @var song %><br />
<% @endloop %>

```

You can also find more resources and examples of the official page : http://common-lisp.net/project/cl-emb/

### Configuration

Caveman uses ".lisp" file as configuration file in `#p"config/"` directory. When a project is just generated, you might be able to find `dev.lisp` in it. It will be used when "start" the project application with "dev" mode.

```common-lisp
;; config/dev.lisp
`(:static-path #p"static/"
  :log-path #p"log/"
  :template-path #p"templates/"
  :application-root ,(asdf:component-pathname
                      (asdf:find-system :myapp))
  :server :hunchentoot
  :port 5000
  :database-type :sqlite3
  :database-connection-spec (,(namestring
                               (asdf:system-relative-pathname
                                :myapp
                                "sqlite3.db"))))
```

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

You can access to the configuration plist anywhere, by using `caveman:config`.

```common-lisp
(caveman:config)
;;=> (:static-path #p"public/" :template-path ...)
(caveman:config :server)
;;=> :hunchentoot
```

### Helper

* `context`
* `with-context-variables`
* `config`
* `app-path`
* `url-for`
* `redirect-to`
* `forward-to`
* `current-uri`
* `current-mode`

### Session

`caveman:*session*` is a hash table which represents a session for the current user.

## More practical

### Extend the Context

### Database

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Contributors

* Tomohiro Matsuyama (tomo@cx4a.org)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## License

Licensed under the LLGPL License.
