;;; ctads-mode.el --- major mode for editing TADS3, derived from CC Mode

;; Author:   George Katsitadze
;; Created:  May 2015
;; Version:  0.1-dev
;; Keywords: tads3 mode
;; X-URL:    https://github.com/geokat/ctads-mode

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;    This is a major mode for editing TADS3 code. The goal is to keep
;;    all the features of c++-mode and provide some TADS3-specific
;;    additions. The mode falls back on CC Modes' c++-mode for most of
;;    its functionality, and so is probably only useful for TADS3 code
;;    written in the 'C' style (using semicolons after property
;;    definitions, braces for object definitions, empty parens for
;;    argument-less function/method defintions and so on).  Even if
;;    you're not excited about those conventions, you might consider
;;    them a small price for the awesome code editing functionality of
;;    CC Mode.
;;
;;    (Note that CC Mode does provide support for line-oriented
;;    languages (where statements can be terminated by EOL--AWK is an
;;    example of such language), but implementing it for TADS3 will
;;    require more work than I'm willing to do at the moment.)
;;
;;    ctads-mode requires CC Mode.  It works with cc-mode 5.32.5,
;;    which is current at this time.

;;; Installation instructions:
;;
;; Put ctads-mode.el somewhere in your load path, optionally byte-compile
;; it, and add the following to your .emacs file:
;;
;;   (autoload 'ctads-mode "ctads-mode" "Major mode for editing TADS3 code" t)
;;   (add-to-list 'auto-mode-alist '("\\.t$" . ctads-mode))
;;
;; You can disable ctads-mode's handling of multiline strings by
;; putting something like this in your .emacs file:
;;
;;   (setq ctads-prettify-multiline-strings nil)

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'ctads-mode 'c++-mode))

(defun ctads-adaptive-fill-function ()
  "Return a fill prefix for multiline strings.
Because TADS3 ignores extraneous whitespace in strings, we can
format them to look nice in code, with paragraphs and alignment."
  (save-excursion
    ;; Emacs narrows the buffer to the contents of the string when
    ;; calling this function, we need to widen it back to get the
    ;; context.
    (widen)
    ;; We're only interested in strings.
    (if (nth 3 (syntax-ppss))
        (ctads-string-fill-prefix))))

(defun ctads-string-fill-prefix ()
  "Build the fill prefix for a continuing line in a multiline
string. The prefix consists of spaces, so that lines in multiline
strings are aligned nicely."
  (save-excursion
    ;; The 8th element is the string start position.
    (let ((bos (nth 8 (syntax-ppss))))
      (goto-char bos)
      ;; Add one extra space to the following lines, so that
      ;; there's a hanging open quote (it looks nicer).
      (make-string (1+ (- bos (point-at-bol))) ? ))))

(defun ctads-newline ()
  "newline-and-indent wrapper: align new lines in a multiline string
on the opening quote."
  (interactive)
  ;; We're only interested in strings.
  (if (nth 3 (syntax-ppss))
      (progn
        ;; Make sure we don't have lines containing only
        ;; whitespace if the user hits RET twice or more.
        (delete-horizontal-space)
        (insert
         (concat "\n" (ctads-string-fill-prefix))))
    (newline-and-indent)))

(defun ctads-indent-command ()
  "c-indent-command wrapper: enable indentation of lines inside a
multiline string, aligning on the opening quote."
  (interactive)
  ;; We need to be inside a multiline string AND it shouldn't be the
  ;; first line (that c-indent-command can handle itself).
  (if (and (nth 3 (syntax-ppss))
           (save-excursion
             (back-to-indentation)
             (nth 3 (syntax-ppss))))
      (let (pos-in-line
            (oldpnt (point)))
        (back-to-indentation)
        (setq pos-in-line (- oldpnt (point)))
        (delete-horizontal-space t)
        (insert (ctads-string-fill-prefix))
        (if (> pos-in-line 0)
            (forward-char pos-in-line)))
    (c-indent-command)))

;; Declaration blocks in TADS3 start with either `class' or a object
;; name followed by a colon.
(c-lang-defconst c-decl-block-key
  ctads "\\(?:class\\|[[:alnum:]_]+ *:\\)\\(?:[^[:alnum:]_]\\)")

;; c++-mode can confuse TADS3 object definitions with labels if the
;; object name is immediately followed by colon (without any
;; whitespace in between). So turn off the label support in c++-mode.
;; It seems to be a good trade-off, since TADS3 doesn't have access
;; labels (e.g `public') and 'goto' labels are rare anyway (an
;; alternative would be to override the `c-forward-label' defun in
;; cc-engine.el by adding TADS3-specific checks).
(c-lang-defconst c-recognize-colon-labels
  ctads nil)

;; The following doesn't work as well as I hoped it would (TADS3
;; nested object definitions can follow a property definition
;; terminated by a semicolon).
;(c-lang-defconst c-label-prefix-re
;  ctads "\\(;\\)")

;; TADS3 doesn't support var declarations following declaration blocks
;; (e.g. \"foo\" in \"class Foo { ... } foo;\").
(c-lang-defconst c-block-decls-with-vars
  ctads nil)

;; TADS3 doesn't need escaped newlines for multiline strings.
(c-lang-defconst c-multiline-string-start-char
  ctads t)

;; TADS3 doesn't support (or need) escaped newlines inside string
;; literals to make them multiline.
(c-lang-defconst c-string-escaped-newlines
  ctads nil)

;; No `public', `private' or `protected'.
(c-lang-defconst c-protection-kwds
  ctads nil)

(c-lang-defconst c-has-bitfields
  ctads nil)

(c-lang-defconst c-modified-constant
  ctads nil)

(c-lang-defconst c-identifier-ops
  ctads nil)

(c-lang-defconst c-opt-identifier-concat-key
  ctads nil)

(c-lang-defconst c-opt-identifier-prefix-key
  ctads nil)

(c-lang-defconst c-after-id-concat-ops
  ctads nil)

(c-lang-defconst c-assignment-operators
  ctads '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|="))

;; FIXME: TADS3 actually does support operator overloading.
;; http://www.tads.org/t3doc/doc/sysman/opoverload.htm
(c-lang-defconst c-overloadable-operators
  ctads nil)

;; FIXME: see above.
(c-lang-defconst c-overloadable-operators-regexp
  ctads nil)

(c-lang-defconst c-primitive-type-kwds
  ctads nil)

(c-lang-defconst c-primitive-type-prefix-kwds
  ctads nil)

(c-lang-defconst c-typedef-kwds
  ctads nil)

(c-lang-defconst c-type-prefix-kwds
  ctads nil)

;; Not type modifiers, actually (TADS3 has no static typing), but this
;; should do for font-locking purposes.
(c-lang-defconst c-type-modifier-kwds
  ctads '("local" "enum"))

(c-lang-defconst c-class-decl-kwds
  ctads '("class"))

(defcustom ctads-prettify-multiline-strings t
  "*Whether to treat multiline strings as blocks of text,
enabling auto-fill, indentation and paragraph formatting."
  :type 'boolean
  :group 'ctads)

(defvar ctads-mode-syntax-table nil
  "Syntax table used in ctads-mode buffers.")
(or ctads-mode-syntax-table
    (setq ctads-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table ctads))))

(defun ctads-mode ()
  "A major mode for editing TADS3 code based on CC Mode."
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table ctads-mode-syntax-table)
  (setq major-mode 'ctads-mode
	mode-name "CTADS"
	local-abbrev-table c++-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c++-mode-map)
  (c-init-language-vars ctads-mode)
  (c-common-init 'c++-mode)
;  (easy-menu-add ctads-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'ctads-mode-hook)

  (if ctads-prettify-multiline-strings
      (progn
        ;; Remove strings from the auto-fill ignore list.
        (setq c-ignore-auto-fill '(cpp code))
        (turn-on-auto-fill)
        (local-set-key (kbd "RET") 'ctads-newline)
        (local-set-key (kbd "TAB") 'ctads-indent-command)
        (set (make-local-variable 'adaptive-fill-function)
             'ctads-adaptive-fill-function)))
  (c-update-modeline))

(provide 'ctads-mode)
