# Common Lisp Checkout Kata 

This is my first attempt at the checkout kata in Common Lisp.

You'll need SBCL and QuickLisp installed to run it.

### Running:

Get SBCL:

```
$ brew install sbcl
```

(Other packages managers and operating systems are available)

Get QuickLisp:

```
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
$ sbcl --load quicklisp.lisp
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
* (quit)
```

Tell QuickLisp where to find systems:

```
$ echo "(:tree (:home "src/lisp/")) " >> vim ~/.config/common-lisp/source-registry.conf.d/projects.conf 
```

Run the kata: 

```
$ sbcl
* (ql:quickload "checkoutkata")
```