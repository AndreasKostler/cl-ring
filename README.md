# cl-ring

cl-ring is a Common Lisp web applications library inspired by Clojure's Ring and hence Python's WSGI and Ruby's Rack. By abstracting the details of HTTP into a simple, unified API, cl-ring allows web applications to be constructed of modular components that can be shared among a variety of applications, web servers, and web frameworks.

The cl-ring spec is closely compliant with the [Ring/SPEC][1] and should hence make porting Ring middleware straight forward.

The SPEC file at the root of this distribution provides a complete description of the cl-ring interface.

[1]: https://github.com/ring-clojure/ring/blob/master/SPEC

## Libraries

* cl-ring-core - essential functions for handling parameters, cookies and more
* cl-ring-devel - functions for developing and debugging cl-ring applications
* cl-ring-hunchentoot-adapter - a cl-ring adapter for the Hunchentoot web server 

## Installation

Using [Quicklisp](https://www.quicklisp.org/), put the cl-ring sources in your local-projects directory and do
```common-lisp
(ql:quickload :cl-ring)
```

## Documentation

* [Wiki](https://github.com/ring-clojure/ring/wiki)
* [API docs](http://ring-clojure.github.com/ring)

## Getting started

Create a new project with [Quickproject](https://github.com/xach/quickproject):

```common-lisp
(ql:quickload "quickproject")
(quickproject (quickproject:make-project "~/src/lisp/my-app/"
                             :depends-on '(cl-ring-hunchentoot-adapter))
```

Next, edit ~/src/lisp/my-app/my-app.lisp:

```common-lisp
(in-package :my-app)

(defun handler (request)
  '((:status . 200)
    (:header . ((:Content-Type . "text/html")))
    (:body "Hello World")))
```

Start your REPL and type

```common-lisp
(use-package ':my-app ':cl-ring-hunchentoot-adapter)
(run-hunchentoot 'handler '(:port 3000))
```

A webserver will now be running at <http://localhost:3000/>

## Thanks

This project borrows heavily from Ring;
thanks to Mark McGranaghan and the Clojure community for their work.

## License

Copyright (c) 2012 Andreas Koestler and released to the Public Domain.
