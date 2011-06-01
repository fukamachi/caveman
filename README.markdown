# Caveman - Web Application Framework for Common Lisp

Caveman is a Web Application Framework for Common Lisp, based on [Clack](https://github.com/fukamachi/clack).

Most of Common Lisp WAF adopt Continuations-based development, but Caveman adopts MVC-style architecture.

## Getting started

    (caveman.skeleton:generate :myapp)

Then a project skeleton is generated to current directory.

    (ql:quickload :myapp)
    (myapp:start)

Now you can access to http://localhost:8080/ and then Caveman may show you "Hello, Caveman!".

## Components

### The Controller

`src/controller.lisp`

    ;; Don't forget calling this.
    (cl-annot:enable-annot-syntax)
    
    @url GET "/"
    (defun index (params)
      (render
       "index.tmpl"
       params))
    
    @url POST "/"
    (defun index-post (params)
      @ignore params
      "Hello, Caveman!")

### The View

Caveman adopt CL-EMB as the default template engine. A package, named `myapp.view.emb`, will be generated in your project which has one function `render`. It is simply execute `emb:execute-emb` and return the result as HTML.

Of course, you can use other template engines, such as "cl-markup".

### The Configuration

See `Caveman.Config`.

### The Context

See `Caveman.Context`.

## Dependency

* [Clack](https://github.com/fukamachi/clack)
* [cl-annot](https://github.com/arielnetworks/cl-annot)
* [cl-syntax](https://github.com/m2ym/cl-syntax)
* CL-PPCRE
* CL-FAD
* CLSQL

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Contributors

* Tomohiro Matsuyama (tomo@cx4a.org)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## License

Licensed under the LLGPL License.
