;;; swift-mode.el --- Major-mode for Apple's Swift programming language. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;; Version: 0.4.0-cvs
;; Package-Requires: ((emacs "24.4"))
;; Keywords: languages swift

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
(require 'comint)

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
  :type 'integer)

(defcustom swift-indent-switch-case-offset 0
  "Defines the indentation offset for cases in a switch statement."
  :group 'swift
  :type 'integer)

(defcustom swift-indent-multiline-statement-offset 2
  "Defines the indentation offset for multiline statements."
  :group 'swift
  :type 'integer
  :package-version '(swift-mode "0.3.0"))

(defcustom swift-repl-executable
  "xcrun swift"
  "Path to the Swift CLI."
  :group 'swift)

;;; Indentation

(require 'smie)

(defconst swift-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (type (type) (type "<T" types "T>") ("[" type "]"))
       (types (type) (type "," type))

       (class-decl-exp (id) (id ":" types))
       (decl-exp (id) (id ":" type))
       (decl-exps (decl-exp) (decl-exp "," decl-exp))

       (assign-exp (decl-exp) (id "=" exp))

       (decl (decl ";" decl))
       (decl (let-decl) (var-decl))
       (let-decl
        ("let" id ":" type)
        ("let" id "=" exp)
        ("let" id ":" type "=" exp))
       (var-decl
        ("var" id ":" type)
        ("var" id "=" exp)
        ("var" id ":" type "=" exp))

       (top-level-sts (top-level-st) (top-level-st ";" top-level-st))
       (top-level-st
        ("import" type)
        (decl)
        ("ACCESSMOD" "class" class-decl-exp "{" class-level-sts "}")
        ("ACCESSMOD" "protocol" class-decl-exp "{" protocol-level-sts "}"))

       (class-level-sts (class-level-st) (class-level-st ";" class-level-st))
       (class-level-st
        (decl)
        (func))

       (protocol-level-sts (protocol-level-st) (protocol-level-st ";" protocol-level-st))
       (protocol-level-st
        (decl)
        (func-decl))

       (func-body (insts) ("return" exp))
       (func (func-decl "{" func-body "}"))
       (func-decl ("DECSPEC" "func" func-header)
                  (func-decl "->" type))
       (func-header (id "(" func-params ")"))
       (func-param (decl-exp) (decl-exp "=" id) ("..."))
       (func-params (func-param "," func-param))

       (insts (inst) (insts ";" insts))
       (inst (decl)
             (exp "=" exp)
             (tern-exp)
             (in-exp)
             (dot-exp)
             (dot-exp "{" closure "}")
             (method-call)
             (method-call "{" closure "}")
             ("enum" decl-exp "{" enum-body "}")
             ("switch" exp "{" switch-body "}")
             (if-clause)
             ("for" for-head "{" insts "}")
             ("while" exp "{" insts "}"))

       (dot-exp (id "." id))

       (method-call (dot-exp "(" method-args ")"))
       (method-args (method-arg) (method-arg "," method-arg))
       (method-arg (exp "," "{" closure "}") (exp))

       (exp (op-exp)
            ("[" decl-exps "]"))
       (in-exp (id "in" exp))
       (guard-exp (exp "where" exp))
       (op-exp (exp "OP" exp))
       (tern-exp (op-exp "?" exp ":" exp))

       (enum-case ("ecase" assign-exp)
                  ("ecase" "(" type ")"))
       (enum-cases (enum-case) (enum-case ";" enum-case))
       (enum-body (enum-cases) (insts))

       (case-exps (exp) (guard-exp))
       (case (case-exps ":" insts))
       (switch-body (case) (case "case" case))

       (for-head (in-exp) (op-exp) (for-head ";" for-head))

       (if-conditional (exp) (let-decl))
       (if-body ("if" if-conditional "{" insts "}"))
       (if-clause (if-body)
                  (if-body "elseif" if-conditional "{" insts "}")
                  (if-body "else" "{" insts "}"))

       (closure (insts) (exp "in" insts) (exp "->" id "in" insts)))
     ;; Conflicts
     '((nonassoc "{") (assoc "in") (assoc ",") (assoc ";") (assoc ":") (right "="))
     '((assoc "in") (assoc "where") (assoc "OP"))
     '((assoc ";") (assoc "ecase"))
     '((assoc "case")))

    (smie-precs->prec2
     '(
       (right "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&="
              "^=" "|=" "&&=" "||=" "=")                       ;; Assignment (Right associative, precedence level 90)
       (left "||")                                             ;; Disjunctive (Left associative, precedence level 110)
       (left "&&")                                             ;; Conjunctive (Left associative, precedence level 120)
       (nonassoc "<" "<=" ">" ">=" "==" "!=" "===" "!==" "~=") ;; Comparative (No associativity, precedence level 130)
       (nonassoc "is" "as" "as?")                              ;; Cast (No associativity, precedence level 132)
       (nonassoc ".." "...")                                   ;; Range (No associativity, precedence level 135)
       (left "+" "-" "&+" "&-" "|" "^")                        ;; Additive (Left associative, precedence level 140)
       (left "*" "/" "%" "&*" "&/" "&%" "&")                   ;; Multiplicative (Left associative, precedence level 150)
       (nonassoc "<<" ">>")                                    ;; Exponentiative (No associativity, precedence level 160)
       ))
    )))

(defun verbose-swift-smie-rules (kind token)
  (let ((value (swift-smie-rules kind token)))
    (message "%s '%s'; sibling-p:%s parent:%s hanging:%s == %s" kind token
             (ignore-errors (smie-rule-sibling-p))
             (ignore-errors smie--parent)
             (ignore-errors (smie-rule-hanging-p))
             value)
    value))

(defvar swift-smie--operators-regexp
  (regexp-opt '("*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|=" "&&=" "||="
                "<" "<=" ">" ">=" "==" "!=" "===" "!==" "~=" "||" "&&"
                "is" "as" "as?" ".." "..."
                "+" "-" "&+" "&-" "|" "^"
                "*" "/" "%" "&*" "&/" "&%" "&"
                "<<" ">>" "??")))

(defvar swift-smie--decl-specifier-regexp
  "\\(?1:class\\|mutating\\|override\\|static\\|unowned\\|weak\\)\\(?:[[:space:]]*func\\)")

(defvar swift-smie--access-modifier-regexp
  (regexp-opt '("private" "public" "internal")))

(defun swift-smie--implicit-semi-p ()
  (save-excursion
    (not (or (memq (char-before) '(?\{ ?\[ ?, ?. ?: ?= ?\())
             ;; Checking for operators form for "?" and "!",
             ;; they can be a part of the type.
             ;; Special case: is? and as? are operators.
             (looking-back "[[:space:]][?!]" (- (point) 2) t)
             ;; ??, is? and as? are operators
             (looking-back "[?][?]\\|as[?]\\|is[?]" (- (point) 3) t)
             ;; "in" operator in closure
             (looking-back "in" (- (point) 2) t)
             ;; Characters placed on the second line in multi-line expression
             (looking-at "[ \n\t]+[.?:]")
             ;; Operators placed on the second line in multi-line expression
             ;; Should respect here possible comments strict before the linebreak
             (looking-at (concat "\\(\/\/.*\\)?\n[[:space:]]*" swift-smie--operators-regexp))
             (and (looking-back swift-smie--operators-regexp (- (point) 3) t)
                  ;; Not a generic type
                  (not (looking-back "[[:upper:]]>" (- (point) 2) t)))
             ))))

(defun swift-smie--forward-token ()
  (skip-chars-forward " \t")
  (cond
   ((and (looking-at "\n\\|\/\/") (swift-smie--implicit-semi-p))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ";")

   ((looking-at "{") (forward-char 1) "{")
   ((looking-at "}") (forward-char 1) "}")

   ((looking-at ",") (forward-char 1) ",")
   ((looking-at ":") (forward-char 1) ":")

   ((looking-at "->") (forward-char 2) "->")

   ((looking-at "<") (forward-char 1)
    (if (looking-at "[[:upper:]]") "<T" "OP"))
   ((looking-at ">") (forward-char 1)
    (if (looking-back "[[:space:]]>" 2 t) "OP" "T>"))

   ((looking-at swift-smie--operators-regexp)
    (goto-char (match-end 0)) "OP")

   ((looking-at swift-smie--decl-specifier-regexp)
    (goto-char (match-end 1)) "DECSPEC")

   ((looking-at swift-smie--access-modifier-regexp)
    (goto-char (match-end 0)) "ACCESSMOD")

   ((looking-at "\\<default\\>")
    (goto-char (match-end 0)) "case")

   ((looking-at "else if")
    (goto-char (match-end 0)) "elseif")

   (t (let ((tok (smie-default-forward-token)))
        (cond
         ((equal tok "case")
          (if (looking-at "\\([\n\t ]\\|.\\)+?\\(where.*[,]\\|:\\)")
              "case"
            "ecase"))
         (t tok))))
   ))

(defun swift-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (swift-smie--implicit-semi-p))
      ";")

     ((eq (char-before) ?\{) (backward-char 1) "{")
     ((eq (char-before) ?\}) (backward-char 1) "}")

     ((eq (char-before) ?,) (backward-char 1) ",")
     ((eq (char-before) ?:) (backward-char 1) ":")

     ((looking-back "->" (- (point) 2) t)
      (goto-char (match-beginning 0)) "->")

     ((eq (char-before) ?<) (backward-char 1)
      (if (looking-at "<[[:upper:]]") "<T" "OP"))
     ((looking-back ">[?!]?" (- (point) 2) t)
      (goto-char (match-beginning 0))
      (if (looking-back "[[:space:]]" 1 t) "OP" "T>"))

     ((looking-back swift-smie--operators-regexp (- (point) 3) t)
      (goto-char (match-beginning 0)) "OP")

     ((looking-back swift-smie--decl-specifier-regexp (- (point) 8) t)
      (goto-char (match-beginning 1)) "DECSPEC")

     ((looking-back swift-smie--access-modifier-regexp (- (point) 8) t)
      (goto-char (match-beginning 0)) "ACCESSMOD")

     ((looking-back "\\<default\\>" (- (point) 9) t)
      (goto-char (match-beginning 0)) "case")

     ((looking-back "else if" (- (point) 7) t)
      (goto-char (match-beginning 0)) "elseif")

     (t (let ((tok (smie-default-backward-token)))
          (cond
           ((equal tok "case")
            (if (looking-at "\\([\n\t ]\\|.\\)+?\\(where.*[,]\\|:\\)")
                "case"
              "ecase"))
           (t tok))))
     )))

(defun swift-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) swift-indent-offset)

    (`(:after . ":") 0)
    (`(:before . ":")
     (cond
      ;; Rule for ternary operator in
      ;; assignment expression.
      ;; Static indentation relatively to =
      ((smie-rule-parent-p "=") 2)
      ;; Rule for the case statement.
      ((smie-rule-parent-p "case") swift-indent-offset)
      ;; Rule for the class definition.
      ((smie-rule-parent-p "class") (smie-rule-parent swift-indent-offset))))

    (`(:after . "{")
     (if (smie-rule-parent-p "switch")
         (smie-rule-parent swift-indent-switch-case-offset)))
    (`(:before . ";")
     (if (smie-rule-parent-p "case")
         (smie-rule-parent swift-indent-offset)))

    ;; Apply swift-indent-multiline-statement-offset only if
    ;; - if is a first token on the line
    (`(:before . ".")
     (when (smie-rule-bolp)
       (if (smie-rule-parent-p "{")
           (+ swift-indent-offset swift-indent-multiline-statement-offset)
         swift-indent-multiline-statement-offset)))

    ;; Apply swift-indent-multiline-statement-offset if
    ;; operator is the last symbol on the line
    (`(:before . "OP")
     (when (and (smie-rule-hanging-p)
                (not (smie-rule-parent-p "OP")))
       (if (smie-rule-parent-p "{")
           (+ swift-indent-offset swift-indent-multiline-statement-offset)
         swift-indent-multiline-statement-offset)))

    ;; Indent second line of the multi-line class
    ;; definitions with swift-indent-offset
    (`(:before . ",")
     (if (smie-rule-parent-p "class")
       (smie-rule-parent swift-indent-offset)))

    ;; Disable unnecessary default indentation for
    ;; "func" and "class" keywords
    (`(:after . ,(or `"func" `"class")) (smie-rule-parent))

    ;; "in" token in closure
    (`(:after . "in")
     (if (smie-rule-parent-p "{")
         (smie-rule-parent swift-indent-offset)))

    (`(:after . "(")
     (if (smie-rule-parent-p "(") 0
       (smie-rule-parent swift-indent-offset)))
    (`(:before . "(")
     (cond
      ((smie-rule-next-p "[") (smie-rule-parent))
      ;; Custom indentation for method arguments
      ((smie-rule-parent-p "." "func") (smie-rule-parent))))

    (`(:before . "[")
     (cond
      ((smie-rule-prev-p "->") swift-indent-offset)
      ((smie-rule-parent-p "[") (smie-rule-parent swift-indent-offset))
      ((smie-rule-parent-p "{") nil)
      (t (smie-rule-parent))))
    (`(:after . "->") (smie-rule-parent swift-indent-offset))
    ))

;;; Font lock

(defvar swift-mode--type-decl-keywords
  '("class" "enum" "protocol" "struct" "typealias"))

(defvar swift-mode--val-decl-keywords
  '("let" "var"))

(defvar swift-mode--context-variables-keywords
  '("self" "super"))

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
    "set" "unowned" "unowned(safe)" "unowned(unsafe)" "weak" "willSet" "convenience"
    "required" "dynamic" "final" "lazy" "optional" "private" "public" "internal"))

(defvar swift-mode--attribute-keywords
  '("class_protocol" "exported" "noreturn"
    "NSCopying" "NSManaged" "objc" "auto_closure"
    "IBAction" "IBDesignable" "IBInspectable" "IBOutlet"))

(defvar swift-mode--keywords
  (append swift-mode--type-decl-keywords
          swift-mode--val-decl-keywords
          swift-mode--context-variables-keywords
          swift-mode--fn-decl-keywords
          swift-mode--misc-keywords
          swift-mode--statement-keywords
          swift-mode--contextual-keywords)
  "Keywords used in the Swift language.")

(defvar swift-mode--constants
  '("true" "false" "nil"))

(defvar swift-font-lock-keywords
  `(
    ;; Keywords
    ;;
    ;; Swift allows reserved words to be used as identifiers when enclosed
    ;; with backticks, in which case they should be highlighted as
    ;; identifiers, not keywords.
    (,(rx-to-string
       `(and (or bol (not (any "`"))) bow
             (group (or ,@swift-mode--keywords))
             eow)
       t)
     1 font-lock-keyword-face)

    ;; Attributes
    ;;
    ;; Highlight attributes with keyword face
    (,(rx-to-string
       `(and "@" bow (or ,@swift-mode--attribute-keywords) eow)
       t)
     0 font-lock-keyword-face)

    ;; Types
    ;;
    ;; Any token beginning with an uppercase character is highlighted as a
    ;; type.
    (,(rx bow upper (* word) eow)
     0 font-lock-type-face)

    ;; Function names
    ;;
    ;; Any token beginning after `func' is highlighted as a function name.
    (,(rx bow "func" eow (+ space) (group bow (+ word) eow))
     1 font-lock-function-name-face)

    ;; Value bindings
    ;;
    ;; Any token beginning after `let' or `var' is highlighted as an
    ;; identifier.
    (,(rx-to-string `(and bow
                           (or ,@swift-mode--val-decl-keywords)
                           eow
                           (+ space)
                           (? "(")
                           (group (+ (or (+ (? ?`) word (? ?`)) ?, space)))
                           (? ")"))
                     t)
       1 font-lock-variable-name-face)

    ;; Use high-visibility face for pattern match wildcards.
    (,(rx (not (any word digit)) (group "_") (or eol (not (any word digit))))
     1 font-lock-negation-char-face)

    ;; Constants
    ;;
    ;; Highlight nil and boolean literals.
    (,(rx-to-string `(and bow (or ,@swift-mode--constants) eow))
     0 font-lock-constant-face)

    ;; Attributes
    ;;
    ;; Use string face for attribute name.
    (,(rx (or bol space)(group "@" (+ word)) eow)
     1 font-lock-string-face)

    ;; Imported modules
    ;;
    ;; Highlight the names of imported modules. Use `font-lock-string-face' for
    ;; consistency with C modes.
    (,(rx bow "import" eow (+ space) (group (+ word)))
     1 font-lock-string-face)

    ;; String interpolation
    ;;
    ;; Highlight interpolation expression as identifier.
    (swift-match-interpolation 0 font-lock-variable-name-face t)
    ))

(defun swift-syntax-propertize-function (start end)
  "Syntactic keywords for Swift mode."
  (let (case-fold-search)
    (goto-char start)
    (remove-text-properties start end '(swift-interpolation-match-data))
    (funcall
     (syntax-propertize-rules
      ((rx (group "\\(" (* (any alnum " ()+-._/*[]!?<>&~!:|^%")) ")"))
       (0 (ignore (swift-syntax-propertize-interpolation)))))
     start end)))

(defun swift-syntax-propertize-interpolation ()
  (let* ((beg (match-beginning 0))
         (context (save-excursion (save-match-data (syntax-ppss beg)))))
    (put-text-property beg (1+ beg) 'swift-interpolation-match-data
                       (cons (nth 3 context) (match-data)))))

(defun swift-match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point) 'swift-interpolation-match-data
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'swift-interpolation-match-data)))
        (if (eq (car value) ?\")
            (progn
              (set-match-data (cdr value))
              t)
          (swift-match-interpolation limit))))))

;;; Imenu

(defun swift-mode--mk-regex-for-def (keyword)
  "Make a regex matching the identifier introduced by KEYWORD."
  (let ((ident (rx (any word nonascii "_") (* (any word nonascii digit "_")))))
    (rx-to-string `(and bow ,keyword eow (+ space) (group (regexp ,ident)))
                  t)))

(defvar swift-mode--imenu-generic-expression
  (list
   (list "Functions" (swift-mode--mk-regex-for-def "func") 1)
   (list "Classes"   (swift-mode--mk-regex-for-def "class") 1)
   (list "Enums"     (swift-mode--mk-regex-for-def "enum") 1)
   (list "Protocols" (swift-mode--mk-regex-for-def "protocol") 1)
   (list "Structs"   (swift-mode--mk-regex-for-def "struct") 1)
   (list "Constants" (swift-mode--mk-regex-for-def "let") 1)
   (list "Variables" (swift-mode--mk-regex-for-def "var") 1))
  "Value for `imenu-generic-expression' in swift-mode.")

;;; Flycheck

(eval-after-load 'flycheck
  (lambda ()
    (flycheck-def-option-var flycheck-swift-sdk-path nil swift
       "A path to the targeted SDK"
       :type '(choice (const :tag "Don't link against sdk" nil)
                      (string :tag "Targeted SDK path"))
       :safe #'stringp)

     (flycheck-def-option-var flycheck-swift-linked-sources nil swift
       "Source files path to link against. Can be glob, i.e. *.swift"
       :type '(choice (const :tag "Don't use linked sources" nil)
                      (string :tag "Linked Sources"))
       :safe #'stringp)

     (flycheck-define-checker swift
       "Flycheck plugin for for Apple's Swift programming language."
       :command ("swift"
                 "-frontend" "-parse"
                 (option "-sdk" flycheck-swift-sdk-path)
                 ;; Swift compiler will complain about redeclaration
                 ;; if we will include original file along with
                 ;; temporary source file created by flycheck.
                 ;; We also don't want a hidden emacs interlock files.
                 (eval
                  (let (source file)
                    (when flycheck-swift-linked-sources
                      (setq source (car (flycheck-substitute-argument 'source 'swift)))
                      (setq file (file-name-nondirectory source))
                      (cl-remove-if-not
                       #'(lambda (path)
                           (and
                            (eq (string-match ".#" path) nil)
                            (eq (string-match file path) nil)))
                       (file-expand-wildcards flycheck-swift-linked-sources)))))
                 "-primary-file" source)
       :error-patterns
       ((error line-start (file-name) ":" line ":" column ": "
               "error: " (message) line-end)
        (warning line-start (file-name) ":" line ":" column ": "
                 "warning: " (message) line-end))
       :modes swift-mode)))

;;; REPL

(defvar swift-repl-buffer nil
  "Stores the name of the current swift REPL buffer, or nil.")

;;;###autoload
(defun swift-mode-run-repl (cmd &optional dont-switch-p)
  "Run a REPL process, input and output via buffer `*swift-repl*'.
If there is a process already running in `*swift-repl*', switch to that buffer.
With argument CMD allows you to edit the command line (default is value
of `swift-repl-executable').
With DONT-SWITCH-P cursor will stay in current buffer.
Runs the hook `swift-repl-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
                         (read-string "Run swift REPL: " swift-repl-executable)
                       swift-repl-executable)))
  (unless (comint-check-proc "*swift-repl*")
    (save-excursion (let ((cmdlist (split-string cmd)))
                      (set-buffer (apply 'make-comint "swift-repl" (car cmdlist)
                                         nil (cdr cmdlist)))
                      (swift-repl-mode))))
  (setq swift-repl-executable cmd)
  (setq swift-repl-buffer "*swift-repl*")
  (unless dont-switch-p
    (pop-to-buffer "*swift-repl*")))

(defun swift-mode-send-region (start end)
  "Send the current region to the inferior swift process.
START and END define region within current buffer"
  (interactive "r")
  (swift-mode-run-repl swift-repl-executable t)
  (comint-send-region swift-repl-buffer start end)
  (comint-send-string swift-repl-buffer "\n"))

(defun swift-mode-send-buffer ()
  "Send the buffer to the Swift REPL process."
  (interactive)
  (swift-mode-send-region (point-min) (point-max)))

(define-derived-mode swift-repl-mode comint-mode "Swift REPL"
  "Major mode for interacting with Swift REPL.

A REPL can be fired up with M-x swift-mode-run-repl.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
swift-repl-mode-hook (in that order).

You can send text to the REPL process from other buffers containing source.
    swift-mode-send-region sends the current region to the REPL process,
    swift-mode-send-buffer sends the current buffer to the REPL process.
")

;;; Mode definition

(defvar swift-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Additional symbols
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?: "_" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)

    ;; Parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; HACK: This is not a correct syntax table definition
    ;; for the braces, but it allows us disable smie indentation
    ;; based on  syntax-table. Default behaviour doesn't work with
    ;; closures in method arguments. For example:
    ;;
    ;; foo.bar(10,
    ;;         closure: {
    ;;         })
    ;;
    ;; With enabled syntax table, smie doesn't respect closing brace, so
    ;; it's impossible to provide custom indentation rules
    (modify-syntax-entry ?\{ "w" table)
    (modify-syntax-entry ?\} "w" table)

    table))

(defvar swift-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") 'swift-mode-run-repl)
    (define-key map (kbd "C-c C-f") 'swift-mode-send-buffer)
    (define-key map (kbd "C-c C-r") 'swift-mode-send-region)
    (easy-menu-define swift-menu map "Swift Mode menu"
      `("Swift"
        :help "Swift-specific Features"
        ["Run REPL" swift-mode-run-repl
         :help "Run Swift REPL"]
        ["Send buffer to REPL" swift-mode-send-buffer
         :help "Send the current buffer's contents to the REPL"]
        ["Send region to REPL" swift-mode-send-region
         :help "Send currently selected region to the REPL"]))
    map)
  "Key map for swift mode.")

;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for Apple's Swift programming language.

\\<swift-mode-map>"
  :group 'swift
  :syntax-table swift-mode-syntax-table
  (setq font-lock-defaults '((swift-font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'swift-syntax-propertize-function)

  (setq-local imenu-generic-expression swift-mode--imenu-generic-expression)

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local electric-indent-chars
              (append '(?. ?, ?: ?\) ?\] ?\}) electric-indent-chars))
  (smie-setup swift-smie-grammar 'swift-smie-rules ;; 'verbose-swift-smie-rules
              :forward-token 'swift-smie--forward-token
              :backward-token 'swift-smie--backward-token))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

(provide 'swift-mode)

;;; swift-mode.el ends here
