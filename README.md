# Cl-Httpbin
A common lisp clone of httpbin by Kenneth Reitz, see httpbin at http://httpbin.org/ , not finished yet. Just to find out the possibility of web development of common lisp and try to practise the skills of common lisp.


## Installation & Usage

If you use quicklisp, you can clone this repo at `~/quicklisp/local-projects` as local project, then

```
(ql:quickload :cl-httpbin)
```

to load it.



After load it, run the server:

```
(cl-httpbin:start)
```

stop the server

```
(cl-httpbin:stop)
```


After load it ,run test as:

```
(asdf:test-system :cl-httpbin)
```

## Author

* Nisen (imnisen@gmail.com)

## Copyright

Copyright (c) 2018 Nisen (imnisen@gmail.com)

## License

Licensed under the BSD License.
