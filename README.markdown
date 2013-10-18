# Caveman2 - Micro web application framework

## Usage

```common-lisp
(defparameter *web* (make-instance '<app>))

(defroute (*web* "/") ()
  (with-layout (:title "Welcome to My site")
    (render #P"index.tmpl")))

(defroute (*web* "/hello") (&key (|user| "Guest"))
  (format nil "Hello, ~A" |user|))
```

## What's the difference from Caveman 1 ?

* Bases on [ningle](http://8arrow.org/ningle/)
* Supports database integration
* New configuration system ([Envy](https://github.com/fukamachi/envy))
* New routing macro

## Design Goal

* Be Small.
* Be Extensible.
* Be Practical.

## Quickstart

You came to here because you're interested in living like a caveman, right? There's no Disneyland, but it's good place to start. Let's get into a cave.

### Generating a project skeleton

```common-lisp
(caveman:make-project #P"/path/to/myapp/"
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

Since Caveman bases on ningle, Caveman also has the [Sinatra](http://www.sinatrarb.com/)-like routing system.

```common-lisp
;; GET request (default)
(defroute (*app* "/" :method :GET) () ...)

;; POST request
(defroute (*app* "/" :method :POST) () ...)

;; PUT request
(defroute (*app* "/" :method :PUT) () ...)

;; DELETE request
(defroute (*app* "/" :method :DELETE) () ...)

;; OPTIONS request
(defroute (*app* "/" :method :OPTIONS) () ...)
```

Route pattern may contain "keyword" to put the value into the argument.

```common-lisp
(defroute (*app* "/hello/:name") (&key name)
  (format nil "Hello, ~A" name))
```

The above controller will be invoked when you access to "/hello/Eitarow" or "/hello/Tomohiro", and then `name` will be "Eitarow" and "Tomohiro".

`(&key name)` is almost same as a lambda list of Common Lisp, excepts it always allows other keys.

```common-lisp
(defroute (*app* "/hello/:name") (&rest params &key name)
  ;; ...
  )
```

Route patterns may also contain "wildcard" parameters. They are accessible by `splat`.

```common-lisp
(defroute (*app* "/say/*/to/*") (&key splat)
  ; matches /say/hello/to/world
  splat ;=> ("hello" "world")
  ))

(defroute (*app* "/download/*.*") (&key splat)
  ; matches /download/path/to/file.xml
  splat ;=> ("path/to/file" "xml")
  ))
```

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

After restarting a server, "Caveman.Middleware.DBIManager" will be enabled. To connect to each database, use `caveman.middleware.dbimanager:connect-to` in `defroute`s.

```common-lisp
(import 'caveman.middleware.dbimanager:connect-to)

(defroute (*web* "/users") ()
  (let ((db (connect-to :maindb)))
    (dbi:execute (dbi:prepare db "SELECT ..."))
    ))
```

The connection is alive during its Lisp session and will be reused for each HTTP requests.

See a documentation of [CL-DBI](http://8arrow.org/cl-dbi/) for more details.

### Set HTTP headers or HTTP status

There are several special variables available during a HTTP request. `*request*` and `*response*` represents a request and a response. If you are familiar with [Clack](http://clacklisp.org/), these are instances of subclasses of [Clack.Request](http://quickdocs.org/clack/api#package-CLACK.REQUEST) and [Clack.Response](http://quickdocs.org/clack/api#package-CLACK.RESPONSE).

```common-lisp
(import '(caveman:*request*
          caveman:*response*
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

### Use session

Session data is for memorizing user-specific data. `*session*` is a hash table represents session data.

### Specify error pages

To specify error pages for 404, 500 or so, define a method `on-exception` of your app.

```common-lisp
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
```

### Start a server

Your application has `start` and `stop` to start/stop your web application. This is a example assuming that the name of your application is "myapp".

```common-lisp
(myapp:start :port 8080)
```

As Caveman bases on Clack, you can choose which server to run on -- Hunchentoot, mod_lisp or FastCGI.

```common-lisp
(myapp:start :server :hunchentoot :port 8080)
(myapp:start :server :fcgi :port 8080)
```

I recommend you to use Hunchentoot in local machine and use FastCGI for production environment.

```common-lisp
(myapp:stop)
```

### Deployment

You can also start your application by using [Shelly](https://github.com/fukamachi/shelly).

    $ APP_ENV=development shly -Lclack clackup app.lisp --server :fcgi --port 8080

Shelly allows you to execute a Common Lisp function like a shell command.

### Hot Deployment

Though Caveman doesn't have a feature for hot deployment, [Server::Starter](http://search.cpan.org/~kazuho/Server-Starter-0.15/lib/Server/Starter.pm) -- a Perl module -- makes it easy.

    $ APP_ENV=production start_server --port 8080 -- shly start --server :fcgi

To restart the server, send HUP signal (`kill -HUP <pid>`) to the `start_server` process.

### Error Log

Caveman outputs error backtraces to a file which is wrote at `:error-log` of your configuration.

```common-lisp
(defconfig |defautl|
  `(:error-log #P"/var/log/apps/myapp_error.log"
    :databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"myapp.db"
                                                        *application-root*)))))
```

## Installation

Caveman depends on the latest revision of Clack, ningle, CL-DBI and Envy.

    $ cd ~/quicklisp/local-projects
    $ git clone https://github.com/fukamachi/clack
    $ git clone https://github.com/fukamachi/ningle
    $ git clone https://github.com/fukamachi/cl-dbi
    $ git clone https://github.com/fukamachi/envy
    $ git clone https://github.com/fukamachi/caveman

## See Also

* [Clack](http://clacklisp.org/) - Web application environment.
* [ningle](http://8arrow.org/ningle/) - Super micro web application framework Caveman bases on.
* [CL-EMB](http://www.common-lisp.net/project/cl-emb/) - HTML Templating engine.
* [CL-DBI](http://8arrow.org/cl-dbi/) - Database independent interface library.
* [Envy](https://github.com/fukamachi/envy) - Configuration switcher.
* [Shelly](https://github.com/fukamachi/shelly) - Script to run Common Lisp from shell.

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the LLGPL License.
