# Caveman2 - Lightweight web application framework

## Usage

```common-lisp
(defparameter *web* (make-instance '<app>))

@route GET "/"
(defun index ()
  (with-layout (:title "Welcome to My site")
    (render #P"index.tmpl")))

@route GET "/hello"
(defun say-hello (&key (|name| "Guest"))
  (format nil "Hello, ~A" |name|))
```

## About Caveman2

### What's the difference from Caveman "1" ?

All of them. Caveman2 was written from scratch.

These are noticeable points.

* Bases on [ningle](http://8arrow.org/ningle/)
* Database integration
* New separated configuration system ([Envy](https://github.com/fukamachi/envy))
* New routing macro

### "Why did you write it from scratch?"

One of the most frequently asked question was "Which should I use ningle or Caveman? What are the differences?" I think it was because the role of them were too similar. Both of them are saying "micro" and no database support.

Caveman2 is no more "micro" web application framework. It supports CL-DBI and has database connection management by default. Caveman has started growing up.

### So, Caveman "1" will be dead?

No. Though Caveman "1" will be inactive project, you can keep using it. I don't wanna break existing Caveman apps.

## Design Goal

Caveman is intended to be a collection of common parts of web applications. Caveman has 3 rules to make decisions.

* Be extensible.
* Be practical.
* Don't force anything.

## Quickstart

You came to here because you're interested in living like a caveman, right? There's no Disneyland, but it's good place to start. Let's get into a cave.

### Installation

Caveman depends on the latest revision of Clack, ningle, CL-DBI, SxQL and Envy.

    $ cd ~/quicklisp/local-projects
    $ git clone https://github.com/fukamachi/clack
    $ git clone https://github.com/fukamachi/ningle
    $ git clone https://github.com/fukamachi/cl-dbi
    $ git clone https://github.com/fukamachi/sxql
    $ git clone https://github.com/fukamachi/envy
    $ git clone https://github.com/fukamachi/caveman

```common-lisp
(ql:quickload :caveman2)
```

### Generating a project skeleton

```common-lisp
(caveman2:make-project #P"/path/to/myapp/"
                       :author "<Your full name>")
;-> writing /path/to/myapp/.gitignore
;   writing /path/to/myapp/README.markdown
;   writing /path/to/myapp/app.lisp
;   writing /path/to/myapp/myapp-test.asd
;   writing /path/to/myapp/myapp.asd
;   writing /path/to/myapp/src/config.lisp
;   writing /path/to/myapp/src/myapp.lisp
;   writing /path/to/myapp/src/view.lisp
;   writing /path/to/myapp/src/web.lisp
;   writing /path/to/myapp/t/myapp.lisp
;   writing /path/to/myapp/templates/_errors/404.html
;   writing /path/to/myapp/templates/index.tmpl
;   writing /path/to/myapp/templates/layout/default.tmpl
```

### Routing

Caveman2 provides 2 ways to define a route -- `@route` and `defroute`. You can choose which to use.

`@route` is an annotation macro defined by using [cl-annot](https://github.com/arielnetworks/cl-annot). It takes a method, an URL-string and a function.

```common-lisp
@route GET "/"
(defun index ()
  ...)

;; A route with no name.
@route GET "/welcome"
(lambda (&key (|name| "Guest"))
  (format nil "Welcome, ~A" |name|))
```

This is similar to Caveman1's `@url` except its argument list. You don't have to specify an argument when you don't need it.

`defroute` is just a macro. It provides same feature to `@route`.

```common-lisp
(defroute index "/" ()
  ...)

;; A route with no name.
(defroute "/welcome" (&key (|name| "Guest"))
  (format nil "Welcome, ~A" |name|))
```

Since Caveman bases on ningle, Caveman also has the [Sinatra](http://www.sinatrarb.com/)-like routing system.

```common-lisp
;; GET request (default)
@route GET "/" (lambda () ...)
(defroute ("/" :method :GET) () ...)

;; POST request
@route POST "/" (lambda () ...)
(defroute ("/" :method :POST) () ...)

;; PUT request
@route PUT "/" (lambda () ...)
(defroute ("/" :method :PUT) () ...)

;; DELETE request
@route DELETE "/" (lambda () ...)
(defroute ("/" :method :DELETE) () ...)

;; OPTIONS request
@route OPTIONS "/" (lambda () ...)
(defroute ("/" :method :OPTIONS) () ...)

;; For all methods
@route ANY "/" (lambda () ...)
(defroute ("/" :method :ANY) () ...)
```

Route pattern may contain "keyword" to put the value into the argument.

```common-lisp
(defroute "/hello/:name" (&key name)
  (format nil "Hello, ~A" name))
```

The above controller will be invoked when you access to "/hello/Eitarow" or "/hello/Tomohiro", and then `name` will be "Eitarow" and "Tomohiro".

`(&key name)` is almost same as a lambda list of Common Lisp, excepts it always allows other keys.

```common-lisp
(defroute "/hello/:name" (&rest params &key name)
  ;; ...
  )
```

Route patterns may also contain "wildcard" parameters. They are accessible by `splat`.

```common-lisp
(defroute "/say/*/to/*" (&key splat)
  ; matches /say/hello/to/world
  splat ;=> ("hello" "world")
  ))

(defroute "/download/*.*" (&key splat)
  ; matches /download/path/to/file.xml
  splat ;=> ("path/to/file" "xml")
  ))
```

Normally, routes are matched in the order they are defined. Only the first route matched is invoked and rest of them just will be ignored. But, a route can punt processing to the next matching route using `next-route`.

```common-lisp
(defroute "/guess/:who" (&key who)
  (if (string= (getf params :who) "Eitarow")
      "You got me!"
      (next-route)))

(defroute "/guess/*" ()
  "You missed!")
```

You can return following formats as the result of `defroute`.

* String
* Pathname
* Clack's response list (containing Status, Headers and Body)

### Templates

Caveman adopts [CL-EMB](http://www.common-lisp.net/project/cl-emb/) for the default templating engine.

```html
<html>
  <head>
    <title><% @var title %></title>
  </head>
  <body>
    <ul>
      <% @loop users %>
      <li><% @var name %></li>
      <% @endloop %>
    </ul>
  </body>
</html>
```

```common-lisp
(import 'myapp.view:render)

(render #P"users.tmpl"
        :users (list ...)
        :has-next-page T)
```

```common-lisp
(with-layout (:title "User List | MyApp")
  (render #P"users.tmpl"
          :users (list ...)
          :has-next-page T))

(with-layout (#P"layout.tmpl" :title "User List | MyApp")
  (render #P"index.tmpl"
          :visitor user))
```

### Static file

Images, CSS, JS, favicon.ico and robot.txt in "static/" directory will be served by default.

```
/images/logo.png => {PROJECT_ROOT}/static/images/logo.png
/css/main.css    => {PROJECT_ROOT}/static/css/main.css
/js/app/index.js => {PROJECT_ROOT}/static/js/app/index.js
/robot.txt       => {PROJECT_ROOT}/static/robot.txt
/favicon.ico     => {PROJECT_ROOT}/static/favicon.ico
```

You can change these rules by rewriting "PROJECT_ROOT/app.lisp". See [Clack.Middleware.Static](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.STATIC) for detail.

### Configuration

Caveman adopts [Envy](https://github.com/fukamachi/envy) as a configuration switcher. It allows to define multiple configurations and to switch them by an environment variable.

This is a typical example.

```common-lisp
(defpackage :myapp.config
  (:use :cl
        :envy))
(in-package :myapp.config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :myapp))))

(defconfig |development|
  '(:debug T
    :databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"test.db"
                                                        *application-root*)))))

(defconfig |production|
  '(:databases
    ((:maindb :mysql :database-name "myapp" :username "whoami" :password "1234")
     (:workerdb :mysql :database-name "jobs" :username "whoami" :password "1234"))))

(defconfig |staging|
  `(:debug T
    ,@|production|))
```

Every configuration is a property list. You can choose the configuration which to use by setting `APP_ENV`.

To get a value from the current configuration, call `myapp.config:config` with a key you want.

```common-lisp
(import 'myapp.config:config)

(setf (osicat:environment-variable "APP_ENV") "development")
(config :debug)
;=> T
```

### Database

If you add `:databases` to the configuration, Caveman enables database support by default. `:databases` is an association list of database settings.

```common-lisp
(defconfig |production|
  '(:databases
    ((:maindb :mysql :database-name "myapp" :username "whoami" :password "1234")
     (:workerdb :mysql :database-name "jobs" :username "whoami" :password "1234"))))
```

After restarting a server, "Caveman.Middleware.DBIManager" will be enabled. To connect to each database, use `caveman2.db:connect-db` in `defroute`s.

```common-lisp
(use-package :caveman2.db)

(defroute "/users" ()
  (let ((db (connect-db :maindb)))
    (select-all db :*
      (from :person)
      (where (:>= :age 20)))
    ))
```

The connection is alive during its Lisp session and will be reused for each HTTP requests.

See documentations of [CL-DBI](http://8arrow.org/cl-dbi/) and [SxQL](https://github.com/fukamachi/sxql) for more details.

### Set HTTP headers or HTTP status

There are several special variables available during a HTTP request. `*request*` and `*response*` represents a request and a response. If you are familiar with [Clack](http://clacklisp.org/), these are instances of subclasses of [Clack.Request](http://quickdocs.org/clack/api#package-CLACK.REQUEST) and [Clack.Response](http://quickdocs.org/clack/api#package-CLACK.RESPONSE).

```common-lisp
(import '(caveman2:*request*
          caveman2:*response*
          clack.request:http-referer
          clack.response:headers
          clack.response:status))

;; Get a value of Referer header.
(http-referer *request*)

;; Set Content-Type header.
(setf (headers *response* :content-type) "application/json")

;; Set HTTP status.
(setf (status *response*) 304)
```

This is an example of a JSON API.

```common-lisp
(import 'yason:encode-plist)

(defroute "/user.json" (&key |id|)
  (setf (headers *response* :content-type) "application/json")
  (let ((person (find-person-from-db |id|)))
    ;; person => (:|name| "Eitarow Fukamachi" :|email| "e.arrows@gmail.com")

    (with-output-to-string (s)
      (yason:encode-plist person s))))

;=> {"name":"Eitarow Fukamachi","email":"e.arrows@gmail.com"}
```

If you would like to set Content-Type "application/json" for all "*.json" requests, `next-route` will help you.

```common-lisp
(defroute "/*.json" ()
  (setf (headers *response* :content-type) "application/json")
  (next-route))

(defroute "/user.json" () ...)
(defroute "/search.json" () ...)
(defroute ("/new.json" :method :POST) () ...)
```

### Use session

Session data is for memorizing user-specific data. `*session*` is a hash table represents session data.

### Throw an HTTP status code

```common-lisp
(import 'caveman2:throw-code)

(defroute ("/auth" :method :POST) (&key |name| |password|)
  (unless (authorize |name| |password|)
    (throw-code 403)))
```

### Specify error pages

To specify error pages for 404, 500 or so, define a method `on-exception` of your app.

```common-lisp
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app code))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
```

### Start a server

Your application has functions named `start` and `stop` to start/stop your web application. This is a example assuming that the name of your application is "myapp".

```common-lisp
(myapp:start :port 8080)
```

As Caveman bases on Clack, you can choose which server to run on -- Hunchentoot, mod_lisp or FastCGI.

```common-lisp
(myapp:start :server :hunchentoot :port 8080)
(myapp:start :server :fcgi :port 8080)
```

I recommend you to use Hunchentoot in local machine and use FastCGI for production environment.

You can also start your application by using [Shelly](https://github.com/fukamachi/shelly).

    $ APP_ENV=development shly -Lclack clackup app.lisp --server :fcgi --port 8080

Shelly allows you to execute a Common Lisp function like a shell command.

### Hot Deployment

Though Caveman doesn't have a feature for hot deployment, [Server::Starter](http://search.cpan.org/~kazuho/Server-Starter-0.15/lib/Server/Starter.pm) -- a Perl module -- makes it easy.

    $ APP_ENV=production start_server --port 8080 -- shly start --server :fcgi

To restart the server, send HUP signal (`kill -HUP <pid>`) to the `start_server` process.

### Error Log

Caveman outputs error backtraces to a file which is specified at `:error-log` in your configuration.

```common-lisp
(defconfig |defautl|
  `(:error-log #P"/var/log/apps/myapp_error.log"
    :databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"myapp.db"
                                                        *application-root*)))))
```

## Use another templating library

### CL-WHO

```common-lisp
(import 'cl-who:with-html-output-to-string)

(defroute "/" ()
  (with-html-output-to-string (output nil :prologue t)
    (:html
      (:head (:title "Welcome to Caveman!"))
      (:body "Blah blah blah."))))
;=> "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
;   <html><head><title>Welcome to Caveman!</title></head><body>Blah blah blah.</body></html>"
```

* [CL-WHO Website](http://weitz.de/cl-who/)

### CL-Markup

```common-lisp
(import 'cl-markup:xhtml)

(defroute "/" ()
  (xhtml
    (:head (:title "Welcome to Caveman!))
    (:body "Blah blah blah.")))
;=> "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><title>Welcome to Caveman!</title></head><body>Blah blah blah.</body></html>"
```

* [CL-Markup repository](https://github.com/arielnetworks/cl-markup)

### cl-closure-template

```html
{namespace myapp.view}

{template renderIndex}
<!DOCTYPE html>
<html>
<head>
  <title>"Welcome to Caveman!</title>
</head>
<body>
  Blah blah blah.
</body>
</html>
{/template}
```

```common-lisp
(import 'myapp.config:*template-directory*)

(closure-template:compile-cl-templates (merge-pathnames #P"index.tmpl"
                                                        *template-directory*))

(defroute "/" ()
  (myapp.view:render-index))
```

* [cl-closure-template](http://quickdocs.org/cl-closure-template/)
* [Closure Templates Documentation](https://developers.google.com/closure/templates/docs/overview)

## Use another database library

### CLSQL

You can use Clack.Middleware.Clsql to use CLSQL in Clack compliant application.

In Caveman, add the middleware to `builder` in "PROJECT_ROOT/app.lisp".

```common-lisp
(ql:quickload :clack-middleware-clsql)
(import 'clack.middleware.clsql:<clack-middleware-clsql>)

(builder
 (<clack-middleware-clsql>
  :database-type :mysql
  :connection-spec '("localhost" "db" "fukamachi" "password"))
 *web*)
```

* [Clack.Middleware.Clsql](http://quickdocs.org/clack/api#system-clack-middleware-clsql)
* [CLSQL: Common Lisp SQL Interface](http://clsql.b9.com/)

### Postmodern

You can use Clack.Middleware.Postmodern to use Postmodern in Clack compliant application.

In Caveman, add the middleware to `builder` in "PROJECT_ROOT/app.lisp".


```common-lisp
(ql:quickload :clack-middleware-postmodern)
(import 'clack.middleware.clsql:<clack-middleware-postmodern>)

(builder
 (<clack-middleware-postmodern>
  :database "database-name"
  :user "database-user"
  :password "database-password"
  :host "remote-address")
 *web*)
```

* [Clack.Middleware.Postmodern](http://quickdocs.org/clack/api#system-clack-middleware-postmodern)
* [Postmodern](http://marijnhaverbeke.nl/postmodern/)

## See Also

* [Clack](http://clacklisp.org/) - Web application environment.
* [ningle](http://8arrow.org/ningle/) - Super micro web application framework Caveman bases on.
* [CL-EMB](http://www.common-lisp.net/project/cl-emb/) - HTML Templating engine.
* [CL-DBI](http://8arrow.org/cl-dbi/) - Database independent interface library.
* [SxQL](http://8arrow.org/sxql/) - SQL builder library.
* [Envy](https://github.com/fukamachi/envy) - Configuration switcher.
* [Shelly](https://github.com/fukamachi/shelly) - Script to run Common Lisp from shell.

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.
