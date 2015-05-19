# ctads-mode

This is a major mode for editing TADS3 code. The goal is to keep
all the features of c++-mode and provide some TADS3-specific
additions. The mode falls back on CC Modes' c++-mode for most of
its functionality, and so is probably only useful for TADS3 code
written in the 'C' style (using semicolons after property
definitions, braces for object definitions, empty parens for
argument-less function/method defintions and so on).  Even if
you're not excited about those conventions, you might consider
them a small price for the awesome code editing functionality of
CC Mode.

ctads-mode requires CC Mode.  It works with cc-mode 5.32.5, which
is current at this time.

## Usage

Put ctads-mode.el somewhere in your load path, optionally byte-compile
it, and add the following to your .emacs file:
```elisp
(autoload 'ctads-mode "ctads-mode" "Major mode for editing TADS3 code" t)
(add-to-list 'auto-mode-alist '("\\.t$" . ctads-mode))
```
 You can disable ctads-mode's handling of multiline strings by
 putting something like this in your .emacs file:

```elisp
(setq ctads-prettify-multiline-strings nil)
```
