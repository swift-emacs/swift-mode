;;; swift-mode.el --- Major-mode for Apple's Swift programming language.

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.2.0") (emacs "24.1"))

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

(require 'dash)
(require 'rx)

;; Font lock.

(defvar swift-mode--type-decl-keywords
  '("class" "enum" "protocol" "struct" "typealias"))

(defvar swift-mode--val-decl-keywords
  '("let" "var"))

(defvar swift-mode--fn-decl-keywords
  '("deinit" "func" "init"))

(defvar swift-mode--misc-keywords
  '("import" "static" "subscript" "extension" "import"))

(defvar swift-mode--statement-keywords
  '("break" "case" "continue" "default" "do" "else" "fallthrough"
    "if" "in" "for" "return" "switch" "where" "while"))

(defvar swift-mode--contextual-keywords
  '("associativity" "didSet" "get" "infix" "inout" "left" "mutating" "none"
    "nonmutating" "operator" "override" "postfix" "precedence" "prefix" "right"
    "set" "unowned" "unowned(safe)" "unowned(unsafe)" "weak" "willSet" "convenience"))

(defvar swift-mode--keywords
  (-flatten (list swift-mode--type-decl-keywords
                  swift-mode--val-decl-keywords
                  swift-mode--fn-decl-keywords
                  swift-mode--misc-keywords
                  swift-mode--statement-keywords
                  swift-mode--contextual-keywords))
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

;; Mode definition.

;; FIXME: Syntax table cobbled together from entries in tuareg-mode and
;; fsharp-mode. Not really tested.
(defvar swift-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "'" st)     ; Make qualified names a single symbol.
    (modify-syntax-entry ?? ". p" st)
    (modify-syntax-entry ?& ". p" st)
    (modify-syntax-entry ?! ". p" st)

    ;; C++-style comments (//)
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    ;; C-style comments (/* */)
    (modify-syntax-entry ?/ "()1n" st)
    (modify-syntax-entry ?*  ". 23n" st)
    (modify-syntax-entry ?/ ")(4n" st)

    (dolist (c '(?$ ?% ?+ ?- ?/ ?: ?< ?= ?> ?@ ?^ ?|))
      (modify-syntax-entry c "." st))

    (modify-syntax-entry ?' "_" st)      ; ' is part of symbols (for primes).
    (modify-syntax-entry ?\" "\"" st)    ; " is a string delimiter
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\) ")(4n" st)
    st)
  "Syntax table for `swift-mode'.")

;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for Apple's Swift programming language.

\\<swift-mode-map>"
  :group 'swift
  (setq-local font-lock-defaults swift-mode--font-lock-defaults)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start-skip (rx "//" (* "/") (* space)))
  )

(provide 'swift-mode)

;;; swift-mode.el ends here
