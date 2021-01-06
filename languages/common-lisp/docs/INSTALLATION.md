*What's needed*

The basic items needed for developing in Common Lisp are:

- A Lisp "Implementation", which will allow you to compile and execute code, as well as supply a REPL.
- A Text editor with facilities for Lisp code. 
- (Nice to have) The often-used reference for the Common Lisp language, the "Common Lisp Hyperspec", available online [here](http://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm)

*Fast Start*

Newbies to CL, particularly ones with previous experience with Emacs, can get an easy & quick start by installing Portacle, the Portable Common Lisp Environment. Free and full featured, it works in all common platforms and comes pre-configured "out of the box". This will supply:

- A Lisp implementation: SBCL, including Quicklisp and ASDF.
- A Text editor: Emacs
... with lots of add-ons for writing with Lisp code: 
- SLIME, the "Superior Lisp Interaction Mode for Emacs" turns Emacs into a Common Lisp IDE
- ParEdit, which makes working with parentheses easy. 

Everything comes already configured out of the box. Install Portacle by downloading from the [front page](https://portacle.github.io/)

Note: Emacs (text editor) can be disorienting at first, if you're not accustomed to it. Fortunately there are many primers on Emacs and SLIME available online. 

*Traditional Start*

If you prefer installing components separately, besides installing your favorite editor for Lisp code, these would be the steps:

Install a lisp implementation such as [SBCL](http://www.sbcl.org/)
or [CLisp](http://clisp.org/). Both can be installed via Homebrew on
Mac OS X.

```bash
brew install sbcl
# or
brew install clisp
```

See their homepages for instructions for installing on other
platforms.

We will use [QuickLisp](http://www.quicklisp.org/beta/#installation) for Lisp library management. To install QuickLisp: 

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
```

Now launch lisp, and copy-paste the following expressions to finish QuickLisp installation:

```lisp
> (load "quicklisp.lisp")         ;; this will load the downloaded lisp file
> (quicklisp-quickstart:install)  ;; this will install quicklisp
> (ql:add-to-init-file)           ;; this will add quicklisp setup to your init file (recommended)
```

See the documentation of your lisp implementation on how to load quicklisp.lisp into lisp.

Optionally load lisp-unit:

```lisp
(ql:quickload "lisp-unit")
```

This will be done the first time you run any exercises tests. But you
can do it now if you want.
