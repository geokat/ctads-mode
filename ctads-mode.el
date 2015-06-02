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

;; Monkey-patch c-forward-label: TADS3 only supports labels in
;; functions (while c++-mode also supports access labels in toplevel).
(defadvice c-forward-label (around ctads-forward-label-advice
                                   activate preactivate)
  (setq ad-return-value
        (if (and (equal major-mode 'ctads-mode)
                 (c-at-toplevel-p))
            nil
          ad-do-it)))

;; Wrap c-beginning-of-statement-1: turn off label search at top
;; (declaration) level.
(defadvice c-beginning-of-statement-1
    (around ctads-beginning-of-statement-1-advice
            activate preactivate)
  (if (equal major-mode 'ctads-mode)
      ;; ignore-labels is an argument of c-beginning-of-statement-1.
      (setq ignore-labels (c-at-toplevel-p)))
  ad-do-it)

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

;; Declaration blocks in TADS3 start with either `class', `modify',
;; `replace' or a object name followed by a colon. For inline object
;; defs it's `object', optionally followed by a colon and inhertiance
;; list). For anonymous object defs it can be a sequence of plus
;; signs (specifying object containment).
(c-lang-defconst c-decl-block-key
  ctads
  (concat "\\(?:[+]+\\|class\\|modify\\|replace\\|object\\|[[:alnum:]_]+ *:\\)"
          "\\(?:[^[:alnum:]_]\\)"))

;; Used for the syntax parsing of inheritance lists and such.
;; (e.g. \"Foo, Bar\" in \"myObject: Foo, Bar { ... }\").
(c-lang-defconst c-class-key
  ctads (c-lang-const c-decl-block-key))

(c-lang-defconst c-class-decl-kwds
  ctads '("class" "object" "modify" "replace"))

;; In addition to the C++ ones, allow '+' for object containment specs
;; and the following chars for 'templated' object defs:
;;
;;   '@': for location/matchObj in topics (e.g. @cave, @torch)
;;   '[': for lists
;;
(c-lang-defconst c-block-prefix-disallowed-chars
  ctads
  (set-difference (c-lang-const c-block-prefix-disallowed-chars)
                  '(?+ ?@ ?\[ ?\] )))

(c-lang-defconst c-recognize-colon-labels
  ctads t)

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

(c-lang-defconst c-type-decl-prefix-key
  ctads (concat "\\([*\(]\\)"
                "\\([^=]\\|$\\)"))

(c-lang-defconst c-brace-list-decl-kwds
  ctads nil)

(c-lang-defconst c-other-block-decl-kwds
  ctads nil)

(c-lang-defconst c-typedef-decl-kwds
  ctads '("class" "modify" "replace"))

(c-lang-defconst c-typeless-decl-kwds
  ctads '("class" "modify" "replace"))

(c-lang-defconst c-colon-type-list-kwds
  ctads '("class" "transient"))

(c-lang-defconst c-modifier-kwds
  ctads nil)

(c-lang-defconst c-decl-hangon-kwds
  ctads nil)

(c-lang-defconst c-nonsymbol-sexp-kwds
  ctads nil)

(c-lang-defconst c-type-list-kwds
  ctads nil)

(c-lang-defconst c-ref-list-kwds
  ctads nil)

(c-lang-defconst c-paren-nontype-kwds
  ctads nil)

(c-lang-defconst c-paren-type-kwds
  ctads nil)

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  ctads '("do" "else" "try" "finally"))

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  ctads '("for" "if" "switch" "while" "foreach" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  "Statement keywords followed by an expression or nothing."
  ctads '("break" "continue" "goto" "return" "exit" "inherited" "throw"))

(c-lang-defconst c-asm-stmt-kwds
  ctads nil)

(c-lang-defconst c-before-label-kwds
  "Keywords that might be followed by a label identifier."
  ctads '("goto" "break" "continue"))

(c-lang-defconst c-constant-kwds
  "Keywords for constants."
  ctads '("true" "nil"))

(c-lang-defconst c-primary-expr-kwds
  "Keywords besides constants and operators that start primary expressions."
  ctads '("self"
          "definingobj"
          "targetobj"
          "targetprop"))

(c-lang-defconst c-lambda-kwds
  "Keywords that start lambda constructs, i.e. function definitions in
expressions."
  ctads '("function" "method"))

(c-lang-defconst c-inexpr-class-kwds
  "Keywords that can start classes inside expressions."
  ctads '("object"))

(c-lang-defconst c-bitfield-kwds
  "Keywords that can introduce bitfields."
  ctads nil)

(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  ctads '("token" "property" "local" "enum" "static" "intrinsic"))

(defcustom ctads-prettify-multiline-strings t
  "*Whether to treat multiline strings as blocks of text,
enabling auto-fill, indentation and paragraph formatting."
  :type 'boolean
  :group 'ctads)

(c-lang-defconst c-basic-matchers-before
  ctads
  `(
    ;; Fontify keyword constants.
    ,@(when (c-lang-const c-constant-kwds)
        (let ((re
               (c-make-keywords-re
                   nil (c-lang-const c-constant-kwds))))
          `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                          1 c-constant-face-name)))))

    ;; Fontify all keywords except the primitive types.
    ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
       1 font-lock-keyword-face)

    ;; Fontify property names in class/object definitions.
    ,(byte-compile
      (lambda (limit)
        (while (re-search-forward
                ;; The ?: catches object names. The ?= is for property
                ;; initializations.
                "\\([[:alpha:]_]+[[:alnum:]_]*\\)[ \n\r\t]*\\(:\\|=\\)"
                limit t)
          (let ((mb1 (match-beginning 1))
                (me1 (match-end 1)))
            (unless
                (c-skip-comments-and-strings limit)
              (if (and (c-at-toplevel-p)
                       (not (get-text-property mb1 'face)))
                  (progn
                    (c-put-font-lock-face
                     mb1 me1 font-lock-variable-name-face)
                    ;; Fontify the inherit type list (e.g. \"Foo\" and
                    ;; \"Bar\" in \"myObject: Foo, Bar { ... }\").  We
                    ;; also look out for 'templated' object defs
                    ;; (which usually have a 'string' and/or @location
                    ;; following the inherit list).
                    (while
                        (and (memq (char-before) '(?: ?,))
                             (skip-chars-forward " \n\r\t")
                             (looking-at
                              "\\([[:alpha:]_]+[[:alnum:]_]*\\)[ \n\r\t]*\\([,{'@]\\)"))
                      (c-put-font-lock-face (match-beginning 1) (match-end 1)
                                            font-lock-type-face)
                      (goto-char (match-end 2))))))))))))

(defconst ctads-font-lock-keywords-1 (c-lang-const c-matchers-1 ctads)
  "Minimal highlighting for CTADS mode.")

(defconst ctads-font-lock-keywords-2 (c-lang-const c-matchers-2 ctads)
  "Fast normal highlighting for CTADS mode.")

(defconst ctads-font-lock-keywords-3 (c-lang-const c-matchers-3 ctads)
  "Accurate normal highlighting for CTADS mode.")

(defvar ctads-font-lock-keywords ctads-font-lock-keywords-3
  "Default expressions to highlight in CTADS mode.")

(defvar ctads-mode-syntax-table nil
  "Syntax table used in ctads-mode buffers.")
(or ctads-mode-syntax-table
    (setq ctads-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table ctads))))

(defvar ctads-mode-abbrev-table nil
  "Abbreviation table used in ctads-mode buffers.")
(c-define-abbrev-table
 'ctads-mode-abbrev-table
 ;; Keywords that if they occur first on a line might alter the
 ;; syntactic context, and which therefore should trig reindentation
 ;; when they are completed.
 '(("else" "else" c-electric-continued-statement 0)
   ("while" "while" c-electric-continued-statement 0)
   ("catch" "catch" c-electric-continued-statement 0)
   ("finally" "finally" c-electric-continued-statement 0)))

(defvar ctads-mode-map (let ((map (c-make-inherited-keymap)))
                         ;; Add bindings which are only useful for TADS3
                         map)
  "Keymap used in ctads-mode buffers.")

(defcustom ctads-mode-hook nil
  "*Hook called by `ctads-mode'."
  :type 'hook
  :group 'ctads)

(defun ctads-mode ()
  "A major mode for editing TADS3 code based on CC Mode."
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table ctads-mode-syntax-table)
  (setq major-mode 'ctads-mode
	mode-name "CTADS"
	local-abbrev-table ctads-mode-abbrev-table
	abbrev-mode t)
  (use-local-map ctads-mode-map)
  (c-init-language-vars ctads-mode)
  (c-common-init 'ctads-mode)
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
