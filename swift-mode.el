;;; swift-mode.el --- Major-mode for Apple's Swift programming language. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major-mode for Apple's Swift programming language.

;;; Code:

(require 'rx)

(eval-and-compile
  ;; Added in Emacs 24.3
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defgroup swift nil
  "Configuration for swift-mode."
  :group 'languages
  :prefix "swift-")

(defcustom swift-indent-offset 4
  "Defines the indentation offset for Swift code."
  :group 'swift
  :type 'integerp)

(defcustom swift-indent-switch-case-offset 0
  "Defines the indentation offset for cases in a switch statement."
  :group 'swift
  :type 'integerp)

;;; Indentation

(defun swift-indent--paren-level ()
  "Return the paren level at point."
  (nth 0 (syntax-ppss)))

(defun swift-indent--in-str-or-cmnt-p ()
  "Non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss)))

(defun swift-indent--back-to-start-of-level ()
  "Move backwards up to the start of the current indentation level."
  (let ((current-level (swift-indent--paren-level)))
    (back-to-indentation)
    (while (> (swift-indent--paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))))

(defun swift-indent--rewind-past-str-cmnt ()
  "Move to the start of the comment at point."
  (goto-char (nth 8 (syntax-ppss))))

(defun swift-indent--rewind-irrelevant ()
  "Move backward past spaces and comments."
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (when (looking-back "\\*/")
      (backward-char))
    (when (swift-indent--in-str-or-cmnt-p)
      (swift-indent--rewind-past-str-cmnt))
    (when (/= starting (point))
      (swift-indent--rewind-irrelevant))))

(defun swift-indent--align-to-expr-after-brace ()
  "Return the column to use for aligning an expression after a brace."
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line.
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
        (forward-word 1)
        (backward-word 1))
      (current-column))))

(defun swift-indent--at-enum-case-p ()
  "Non-nil if point is at a case keyword at the top level of an enum declaration."
  (save-excursion
    (back-to-indentation)
    (when (looking-at (rx bow "case" eow))
      (backward-up-list)
      (swift-indent--back-to-start-of-level)
      (looking-at (rx bow "enum" eow)))))

(defun swift-indent--calculate-indentation ()
  "Calculate the indentation column to use for `swift-indent-line'.
Returns the column number as an integer."
  (save-excursion
    (back-to-indentation)
    ;; Point is now at beginning of line.
    (let* ((level (swift-indent--paren-level))
           ;; Our "baseline" is one level out from the indentation of the
           ;; expression containing the innermost enclosing opening bracket.
           ;; That way if we are within a block that has a different indentation
           ;; than this mode would give it, we still indent the inside of it
           ;; correctly relative to the outside.
           (baseline
            (if (zerop level)
                0
              (save-excursion
                (backward-up-list)
                (swift-indent--back-to-start-of-level)
                (+ (current-column) swift-indent-offset)))))
      (cond
       ;; A function return type is indented to the corresponding function arguments
       ((looking-at "->")
        (save-excursion
          (backward-list)
          (or (swift-indent--align-to-expr-after-brace)
              (+ baseline swift-indent-offset))))

       ;; A closing brace is 1 level unindented
       ((looking-at "}") (- baseline swift-indent-offset))

       ;; Doc comments in /** style with leading * indent to line up the *s
       ((and (nth 4 (syntax-ppss)) (looking-at "*"))
        (+ 1 baseline))

       ;; If we're in any other token-tree / sexp, then:
       (t
        (or
         ;; If we are inside a pair of braces, with something after the
         ;; open brace on the same line and ending with a comma, treat
         ;; it as fields and align them.
         (when (> level 0)
           (save-excursion
             (swift-indent--rewind-irrelevant)
             (backward-up-list)
             ;; Point is now at the beginning of the containing set of braces
             (swift-indent--align-to-expr-after-brace)))

         (progn
           (back-to-indentation)
           ;; Point is now at the beginning of the current line
           (cond
            ((swift-indent--at-enum-case-p)
             baseline)
            ;; Cases are indented to the same level as the enclosing switch
            ;; statement, plus a user-customisable offset.
            ((looking-at (rx bow (or "case" "default") eow))
             (+ (- baseline swift-indent-offset)
                swift-indent-switch-case-offset))
            (t
             baseline)))))))))

(defun swift-indent-line ()
  "Indent the current line.  Also see `swift-indent-offset'."
  (interactive "*")
  (let ((indent (swift-indent--calculate-indentation)))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent)
      (save-excursion
        (indent-line-to indent)))))

;;; Font lock

(defvar swift-mode--type-decl-keywords
  '("class" "enum" "protocol" "struct" "typealias"))

(defvar swift-mode--val-decl-keywords
  '("let" "var"))

(defvar swift-mode--fn-decl-keywords
  '("deinit" "func" "init"))

(defvar swift-mode--misc-keywords
  '("import" "static" "subscript" "extension"))

(defvar swift-mode--statement-keywords
  '("break" "case" "continue" "default" "do" "else" "fallthrough"
    "if" "in" "for" "return" "switch" "where" "while"))

(defvar swift-mode--contextual-keywords
  '("associativity" "didSet" "get" "infix" "inout" "left" "mutating" "none"
    "nonmutating" "operator" "override" "postfix" "precedence" "prefix" "right"
    "set" "unowned" "unowned(safe)" "unowned(unsafe)" "weak" "willSet" "convenience"))

(defvar swift-mode--keywords
  (append swift-mode--type-decl-keywords
          swift-mode--val-decl-keywords
          swift-mode--fn-decl-keywords
          swift-mode--misc-keywords
          swift-mode--statement-keywords
          swift-mode--contextual-keywords)
  "Keywords used in the Swift language.")

(defvar swift-mode--constants
  '("true" "false" "nil"))

(defvar swift-mode--font-lock-defaults
  (list
   (list

    ;; Keywords
    ;;
    ;; Swift allows reserved words to be used as identifiers when enclosed
    ;; with backticks, in which case they should be highlighted as
    ;; identifiers, not keywords.
    (cons (rx-to-string
           `(and (or bol (not (any "`"))) bow
                 (group (or ,@swift-mode--keywords))
                 eow)
           t)
          1)

    ;; Types
    ;;
    ;; Any token beginning with an uppercase character is highlighted as a
    ;; type.
    (cons (rx bow upper (* word) eow)
          font-lock-type-face)

    ;; Function names
    ;;
    ;; Any token beginning after `func' is highlighted as a function name.
    (cons (rx bow "func" eow (+ space) (group bow (+ word) eow))
          (list 1 font-lock-function-name-face))

    ;; Value bindings
    ;;
    ;; Any token beginning after `let' or `var' is highlighted as an
    ;; identifier.
    (cons (rx-to-string `(and bow
                              (or ,@swift-mode--val-decl-keywords)
                              eow
                              (+ space)
                              (group (? "`") bow (+ word) eow (? "`")))
                        t)
          (list 1 font-lock-variable-name-face))

    ;; Use high-visibility face for pattern match wildcards.
    (cons (rx (not (any word digit)) (group "_") (or eol (not (any word digit))))
          (list 1 font-lock-negation-char-face))

    ;; Constants
    ;;
    ;; Highlight nil and boolean literals.
    (cons (rx-to-string `(and bow (or ,@swift-mode--constants) eow))
          font-lock-constant-face)

    ;; Attributes
    ;;
    ;; Use string face for attribute name.
    (cons (rx (or bol space)(group "@" (+ word)) eow)
          (list 1 font-lock-string-face))

    ;; Imported modules
    ;;
    ;; Highlight the names of imported modules. Use `font-lock-string-face' for
    ;; consistency with C modes.
    (cons (rx bow "import" eow (+ space) (group (+ word)))
          (list 1 font-lock-string-face)))))

;;; Mode definition

;; HACK: This syntax table is lifted directly from `rust-mode'. There may be
;; corner cases in the Swift syntax that are not accounted for.
(defvar swift-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; _ is a word-char
    (modify-syntax-entry ?_ "w" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for Apple's Swift programming language.

\\<swift-mode-map>"
  :group 'swift
  :syntax-table swift-mode-syntax-table
  (setq-local font-lock-defaults swift-mode--font-lock-defaults)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local tab-width swift-indent-offset)
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function 'swift-indent-line)

  (setq-local comment-start-skip
              (rx (or (and "//" (* "/")) (and "/*" (* "*"))) (* space)))

  (setq-local paragraph-start
              (rx-to-string `(and (* space)
                                  (or (regex ,comment-start-skip)
                                      (and "*" (? "/") (* space)))
                                  eol)
                            t))

  (setq-local paragraph-separate paragraph-start))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

(provide 'swift-mode)

;;; swift-mode.el ends here
