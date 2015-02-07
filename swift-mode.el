;;; swift-mode.el --- Major-mode for Apple's Swift programming language. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;; Version: 0.4.0-cvs
;; Package-Requires: ((emacs "24.1"))
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
  :type 'integerp)

(defcustom swift-indent-supertype-offset 4
  "Defines the indentation offset for supertypes.

example:
class Foo:
    Bar ← offset for this line"
  :group 'swift
  :type 'integerp
  :package-version '(swift-mode "0.4.0"))

(defcustom swift-indent-switch-case-offset 0
  "Defines the indentation offset for cases in a switch statement."
  :group 'swift
  :type 'integerp)

(defcustom swift-indent-multiline-statement-offset 2
  "Defines the indentation offset for for multiline statements."
  :group 'swift
  :type 'integerp
  :package-version '(swift-mode "0.3.0"))

(defcustom swift-repl-executable
  "xcrun swift"
  "Path to the Swift CLI."
  :group 'swift)

;;; Indentation

(require 'smie)

(defconst swift-smie-prec2
  (smie-bnf->prec2
   '(;; identifiers and other insignificant tokens.
     ;; SMIE automatically skips tokens not mentioned in the syntax.
     (id)
     ;; instructions or something that may appers in blocks.
     (insts
      (exprs)
      (func)
      (class)
      (enum)
      (switch)
      (case)
      (default)
      (for)
      (insts ";" insts)
      (insts "IMP;" insts))
     ;; expressions.
     (exprs (expr) (exprs "," exprs))
     (expr
      (id)
      (expr "OP" expr)
      (expr "=" expr)
      (expr "?" expr ":" expr)
      ("[" exprs "]"))
     ;; function declaration.
     (func (func-modifiers "func" params))
     (func-modifiers (id) ("class"))
     ;; function parameters.
     (params ("(" exprs ")"))
     ;; class declaration.
     (class (class-modifiers "class" id) (id "class" id "TYPE:" types))
     ;; enum declaration.
     (enum (class-modifiers "enum" id) (id "enum" id "TYPE:" types))
     (class-modifiers (id))
     ;; switch statement header.
     (switch (id "switch" expr))
     ;; case label.
     (case (id "case" id ":" id))
     (default (id "default" id ":" id))
     ;; for statement.
     (for (id "for" expr ";" expr ";" expr) (id "for" expr "in" expr))
     ;; block.
     (block ("{" insts "}"))
     ;; types.
     (types (type) (types "," types))
     (type
      (id)
      (id "." type)
      ("[" dict-type-inner "]")
      (type "OP" type) ; -> or !
      (type "?" id)
      ("(" types ")")
      ("<T" types "T>") ; generic type. this is almost identical to (id "<T" types "T>") except "<T" and "T>" are recognized as a parenthesis.
      )
     (dict-type-inner (type ":" type)))
   '(;; explicit preceeding definitions for conflict resolution
     (assoc ";" "IMP;")
     (assoc ",")
     (right "=")

     ;; i.e. a ? a ? a : a ? a : a : a == a ? (a ? a : (a ? a : a)) : a
     (left ":")
     (right "?")
     (assoc "OP")
     (left ".")
     )
   ))

;; (insert (prin1-to-string swift-smie-prec2))

(defconst swift-smie-grammar (smie-prec2->grammar swift-smie-prec2))

;; (insert (prin1-to-string swift-smie-grammar))


(defun verbose-swift-smie-rules (kind token)
  (let ((value (swift-smie-rules kind token)))
    (message "%s '%s'; sibling-p:%s parent:%s hanging:%s point: %s == %s" kind token
             (ignore-errors (smie-rule-sibling-p))
             (ignore-errors (smie-indent--parent))
             (ignore-errors (smie-rule-hanging-p))
             (ignore-errors (point))
             value)
    value))

(defvar swift-smie--operators-regexp "\\`[-.!#$%&=^~\\|@+:*<>?]+\\'")

(defun swift-smie--implicit-semi-p ()
  "Return t if the cursor is at the end of a statement."
  (save-excursion
    (or
     ;; inserts implicit semicolon just after a ">" token of a type parameter
     ;; unless a ":", "(", or "=" follows:
     ;;
     ;; protocol {
     ;;   func foo: Foo<A> ← inserts ";" here
     ;;   func bar: Bar<A> ← inserts ";" here
     ;; }
     ;;
     ;; class Foo<A> ← not inserts ";" here
     ;;     : Bar
     ;;
     ;; func foo<A> ← not inserts ";" here
     ;;   (bar: Bar)
     ;;
     ;; let foo: Foo<A> ← not inserts ";" here
     ;;   = new Foo<A>()
     ;;
     (and (looking-back "[[:alnum:]_][\])>]*>" (- (point) 32) nil) ; see comment for T> in swift-smie--forward-token
             (save-excursion
               (forward-comment (point-max))
               (not (looking-at "[:(=]"))))
     ;; sentinel
     (= (point) (point-min))
     (not (or
           ;; supresses implicit semicolon on empty line
           (save-excursion
             (goto-char (line-beginning-position))
             (< (line-end-position) (progn (forward-comment (point-max)) (point))))
           ;; supresses implicit semicolon after operator
           (looking-back "[-.({[,!#$%&=^~\\|@+:*<>?;]" (- (point) 1) t)
           ;; supresses implicit semicolon after keyword
           ;; Note that "as?" is already handled by preceeding conditions.
           (save-excursion
             (member (smie-default-backward-token) '("as" "is" "class" "deinit" "enum" "extension" "func" "import" "init" "internal" "let" "operator" "private" "protocol" "public" "static" "struct" "subscript" "typealias" "var" "case" "for" "if" "return" "switch" "where" "while" "associativity" "convenience" "dynamic" "didSet" "final" "get" "infix" "inout" "lazy" "mutating" "nonmutating" "optional" "override" "postfix" "precedence" "prefix" "required" "set" "unowned" "weak" "willSet")))
           ;; supresses implicit semicolon before operator
           (progn
             (forward-comment (point-max))
             (looking-at "[-.,!#$%&=^~\\|+:*<>?]"))
           ;; supresses implicit semicolon before keyword
           (save-excursion
             ;; note that comments are already skipped by previous condition
             (member (smie-default-forward-token) '("as" "is"))))))))

(defun swift-smie--is-type-colon ()
  "Return t if a colon at the cursor is the colon for supertype declaration or type declaration of let or var."
  (save-excursion
    (or (equal (smie-default-backward-token) ">")
        (member (smie-default-backward-token) '("class" "let" "var")))))

(defun swift-smie--forward-token ()
  (let ((pos (point)))
    (forward-comment (point-max))
    (cond
     ;; implicit semicolon.
     ((and (< pos (line-beginning-position))
           (save-excursion (goto-char pos) (swift-smie--implicit-semi-p)))
      "IMP;")

     ;; separators and parenthesis.
     ((memq (char-after) '(?, ?\; ?\{ ?\} ?\[ ?\] ?\( ?\)))
      (forward-char 1)
      (string (char-before)))

     ;; open angle bracket for type parameters.
     ;; We use a heuristic: spaces are inserted around inequality sign and
     ;; a type paramer starts with an upper case character, a square bracket,
     ;; or a parenthesis.
     ((and (eq (char-after) ?<) (looking-at "<[[:upper:]\[(]"))
      (forward-char 1)
      "<T")

     ;; close angle bracket for type parameters.
     ((and (eq (char-after) ?>)
           (looking-back "[[:alnum:]_][\])>]*" (- (point) 32) nil))
      (forward-char 1)
      "T>")

     (t (let
            ((pos-after-comment (point))
             (tok (smie-default-forward-token)))
          (cond
           ((string-match-p ">+:" tok) ; e.g. class Foo<A: B<C>>:
            (goto-char pos-after-comment)
            (forward-char 1)
            ">")
           ((equal tok ":")
            (save-excursion
              (if (or (equal (smie-default-backward-token) ">:")
                      (swift-smie--is-type-colon))
                  "TYPE:" ":")))
           ((equal tok "?")
            ;; heuristic for conditional operator
            (if (memq (char-before (1- (point))) '(?\s ?\t ?\n))
                "?" ; conditional operator
              "ID" ; part of type name or chaining operator
              ))
           ((equal tok "=") "=")
           ((equal tok "as")
            (when (eq (char-after) ??) ; "as?"
              (forward-char))
            "OP")
           ((equal tok "is") "OP")
           ((equal tok "OP") "ID")
           ((string-match-p swift-smie--operators-regexp tok) "OP")
           (t tok)))))))

(defun swift-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ;; implicit semicolon.
     ((and (> pos (line-end-position))
           (swift-smie--implicit-semi-p))
      "IMP;")

     ;; separators and parenthesis.
     ((memq (char-before) '(?, ?\; ?\{ ?\} ?\[ ?\] ?\( ?\)))
      (backward-char 1)
      (string (char-after)))

     ;; open angle bracket for type parameters.
     ;; We use a heuristic: spaces are inserted around inequality sign and
     ;; a type paramer starts with an upper case character, a square bracket,
     ;; or a parenthesis.
     ((and (eq (char-before) ?<) (looking-at "[[:upper:]\[(]"))
      (backward-char 1)
      "<T")

     ;; close angle bracket for type parameters.
     ((and (eq (char-before) ?>)
           (looking-back "[[:alnum:]_][\])>]*" (- (point) 32) nil))
      (backward-char 1)
      "T>")

     (t (let
            ((pos-before-comment (point))
             (tok (smie-default-backward-token)))
          (cond
           ((string-match-p ">+:" tok) ; e.g. class Foo<A: B<C>>:
            (goto-char pos-before-comment)
            (backward-char 1)
            "TYPE:")
           ((equal tok ":")
            (if (swift-smie--is-type-colon) "TYPE:" ":"))
           ((equal tok "?")
            (let ((pos (point)))
              (if (equal (smie-default-backward-token) "as") ; "as?"
                  "OP"
                (goto-char pos)
                ;; heuristic for conditional operator
                (if (memq (char-before) '(?\s ?\t ?\n))
                    "?" ; conditional operator
                  "ID" ; part of type name or chaining operator
                  ))))
           ((equal tok "=") "=")
           ((member tok '("is" "as")) "OP")
           ((equal tok "OP") "ID")
           ((string-match-p swift-smie--operators-regexp tok)
            "OP")
           (t tok)))))))

(defun swift-smie--backward-sexps-until (tokens
                                         &optional
                                         stop-at-bol-parent-tokens
                                         stop-at-bol-self-tokens)
  "Backward sexps until one of given tokens.
When this function returns, the cursor is at just after one of given tokens.
The form of return values are same as one of `smie-backward-sexp`.

TOKENS is a list of guard tokens until one of that this function backs.
STOP-AT-BOL-PARENT-TOKENS is a list of tokens that if we hit the beginning of
a line just after one of given token, the function returns.
Typically, this is a list of list separator tokens (e.g. ',', ';').
STOP-AT-BOL-SELF-TOKENS is a list of tokens that if we hit the beginning of
a line just before one of given token, the function returns.
Typically, this is a list of list element starter tokens (e.g. 'case' of switch statement body)."
  (let*
      ((pos (point))
       (parent (smie-backward-sexp))
       (parent-pos (nth 1 parent))
       (token (nth 2 parent)))
    (while (or
            ;; we skipped over a paren-like pair or we skipped over an
            ;; identifier, matched parentheses, ...
            (null (car parent))
            (and
             (not (member token tokens))
             (not (eq parent-pos (point)))
             (not (and stop-at-bol-parent-tokens
                       (member token stop-at-bol-parent-tokens)
                       (smie-indent--bolp-1)
                       (/= pos (point))))
             (not (and stop-at-bol-self-tokens
                       (save-excursion
                         (and
                          (member (swift-smie--backward-token)
                                  stop-at-bol-self-tokens)
                          (smie-indent--bolp-1)))
                       (/= pos (point))))))
      (when parent-pos (goto-char parent-pos))
      (setq parent (smie-backward-sexp))
      (setq parent-pos (nth 1 parent))
      (setq token (nth 2 parent)))
    (when (eq (point) (point-min))
      (forward-comment (point-max)))
    parent))

(defun swift-smie--align-with (parent &optional offset)
  "Align with given parent token with optional offset."
  (save-excursion
    (goto-char (nth 1 parent))
    (cons 'column (+ (or offset 0) (smie-indent-virtual)))))

(defun swift-smie--indent-keyword (parents &optional offset)
  "Indent based on outer open-paren or previous statement/declaration.

PARENTS is a list of parent tokens (i.e. open-paren or statement separator).
OFFSET is a offset from parent tokens, or 0 if omitted."
  (save-excursion
    (let* ((pos (point))
           (parent (swift-smie--backward-sexps-until parents)))
      (if (and (eq (point) pos) (smie-indent--bolp-1))
          nil
        (cons 'column (+ (or offset 0) (current-column)))))))

(defvar swift-smie--statement-parent-tokens
  '("IMP;" ";" "{" "(" "[")
  "Parent tokens for statements.")

(defvar swift-smie--expression-parent-tokens
  (append swift-smie--statement-parent-tokens '("," "<T"))
  "Parent tokens for expressions.")

(defun swift-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) swift-indent-multiline-statement-offset)
    (`(:after . "{") swift-indent-offset)
    (`(:after . "(") swift-indent-multiline-statement-offset)
    (`(:after . "[") swift-indent-offset)
    (`(:before . "{")
     (or
      (swift-smie--rule-for) ; special handling for for-statement.
      (swift-smie--indent-keyword swift-smie--statement-parent-tokens)))
    (`(:before . ,(or "(" "["))
     (swift-smie--indent-keyword
      swift-smie--expression-parent-tokens
      (if (smie-indent--bolp-1)
          swift-indent-multiline-statement-offset
        0)))
    (`(:after . ,(or "OP" "?" "="))
     (swift-smie--rule-after-op))
    (`(:before . ,(or "OP" "?" "="))
     (swift-smie--rule-before-op))
    (`(:after . "TYPE:")
     (swift-smie--rule-after-op nil swift-indent-supertype-offset))
    (`(:before . "TYPE:")
     (swift-smie--rule-before-op nil swift-indent-supertype-offset))

    (`(:after . ":")
     ;; If this is a ":" token of case or default clause, indent with standard
     ;; width:
     ;;
     ;; switch {
     ;; case 0:   ← after this ":" token,
     ;;     foo() ← indent with swift-indent-offset
     ;;
     ;; otherwise (i.e. conditional operator or key-value separator), indent
     ;; like other operators.
     (let
         ((pos (point))
          (parent (swift-smie--backward-sexps-until
                   ;; remove "," to skip "," tokens in case label.
                   (append (remove "," swift-smie--expression-parent-tokens)
                           '("case" "default")))))
       (if (member (nth 2 parent) '("case" "default"))
           (progn
             (goto-char (nth 1 parent))
             (cons 'column (+ (current-column) swift-indent-offset)))
         (goto-char pos)
         (swift-smie--rule-after-op t))))

    (`(:before . ":")
     ;; If this is a ":" token of a conditional expression, aligns with the "?"
     ;; token.
     ;; Note that "?" which is not a conditional operator is converted to a
     ;; "ID" token by the lexer.
     (let
         ((pos (point))
          (bolp (smie-indent--bolp-1))
          (parent (swift-smie--backward-sexps-until
                   (swift-smie--op-parent-tokens))))
       (if (and bolp (equal (nth 2 parent) "?"))
           (progn (goto-char (nth 1 parent))
                  (cons 'column (current-column)))
         (goto-char pos)
         (swift-smie--rule-before-op t))))

    (`(:before . ",")
     ;; aligns with the first element of a list or a preceeding sibling that is
     ;; at the beginning of a line.
     (let
         ((pos (point))
          (parent (swift-smie--backward-sexps-until
                   ;; include "case" to indent like
                   ;;
                   ;; switch foo {
                   ;; case bar,
                   ;;      buz:
                   ;;
                   ;; rather than
                   ;;
                   ;; switch foo {
                   ;; case bar,
                   ;; buz:
                   (append (remove "," swift-smie--expression-parent-tokens)
                           '("TYPE:" "case"))
                   '(","))))
       (cons
        'column
        (if (equal (nth 2 parent) "TYPE:")
            ;; class Foo: Bar,
            ;;     Buz
            ;;
            ;; rather than
            ;;
            ;; class Foo: Bar,
            ;;            Buz
            ;;
            ;; If you prefer latter indentation, replace this if-expression
            ;; with a (current-column).
            (progn
              (goto-char (nth 1 parent))
              (+ (smie-indent-virtual) swift-indent-supertype-offset))
          (current-column)))))

    (`(:before . "IMP;")
     (or
      ;; special handling for for-in statement:
      ;;
      ;; for x in
      ;;     xs
      (save-excursion
        (and
         (save-excursion (equal (swift-smie--backward-token) "in"))
         (progn (forward-char 1)
                (swift-smie--backward-for-head))
         (progn
           (swift-smie--forward-token)
           (swift-smie--forward-token)
           (swift-smie--backward-token)
           (cons 'column (current-column)))))
      (swift-smie--rule-before-semicolon t)))

    (`(:before . ";")
     (swift-smie--rule-before-semicolon nil))

    (`(:before . "in")
     (when (swift-smie--backward-for-head)
       (swift-smie--forward-token)
       (swift-smie--forward-token)
       (swift-smie--backward-token)
       (cons 'column (current-column))))
    (`(:after . ,(or "class" "func" "enum" "switch" "case" "for"))
     ;; i.e.
     ;;
     ;; switch
     ;;   longExpression { ← indent of this line
     swift-indent-multiline-statement-offset)

    (`(:before . ,(or "case" "default"))
     (let* ((parent (swift-smie--backward-sexps-until
                     '("{")
                     '()
                     '("case" "default")))
            (pos (nth 1 parent))
            (token (nth 2 parent)))
       (cond
        ((member token '("case" "default"))
         ;; align with previous case label.
         (goto-char pos)
         (cons 'column (current-column)))
        ((equal token "{")
         (goto-char pos)
         (setq parent (swift-smie--backward-sexps-until '("switch" "enum")))
         (when (equal (nth 2 parent) "switch")
           (swift-smie--indent-keyword
            swift-smie--statement-parent-tokens
            swift-indent-switch-case-offset)))
        (t nil))))
    ))

(defun swift-smie--rule-before-semicolon (implicit)
  ;; aligns with the first statement or a preceeding sibling that is at the
  ;; beginning of a line unless that is a case label.
  ;;
  ;; indents with `swift-indent-offset` after case label.
  (or
   (swift-smie--rule-for)
   (let
       ((pos (point))
        (parent (swift-smie--backward-sexps-until
                 (list "{" "(" "[" ":" (if implicit ":" "for"))
                 '("IMP;" ";"))))
     (cons 'column
           (cond
            ((equal (nth 2 parent) ":")
             (setq parent (swift-smie--backward-sexps-until
                           '("{" "(" "[" "case" "default")
                           '("IMP;" ";")))
             (if (member (nth 2 parent) '("case" "default"))
                 (progn
                   (goto-char (nth 1 parent))
                   (+ (current-column) swift-indent-offset))
               (current-column)))
            (t
             (current-column)))))))

(defun swift-smie--backward-for-head ()
  "Backward the head of a for-statement (i.e. 'for foo; bar; buz' or
'for x in xs') and returns the column of the \"for\" token.
If the cursor is not at a head of a for-statement, keeps cursor position as is
and returns nil"
  (let*
      ((pos (point))
       (parent (swift-smie--backward-sexps-until
                swift-smie--statement-parent-tokens))
       (token (nth 2 parent))
       (result-column
        (cond
         ;; 'for foo; bar; |buz' where '|' represents the cursor
         ((equal token ";")
          (goto-char (nth 1 parent))
          (setq parent (swift-smie--backward-sexps-until
                        swift-smie--statement-parent-tokens))
          (setq token (nth 2 parent))
          (when  (equal token ";")
            (goto-char (nth 1 parent))
            (swift-smie--backward-sexps-until
             swift-smie--statement-parent-tokens)
            (when (equal (swift-smie--forward-token) "for")
              (swift-smie--backward-token)
              (current-column))))
         ((equal token "IMP;")
          ;; four cases:
          ;;
          ;; 1.
          ;;   for x
          ;;       in
          ;;       |xs
          ;;
          ;; 2.
          ;;   for x in
          ;;       |xs
          ;;
          ;; 3.
          ;;   for x
          ;;       |in xs
          ;; 4.
          ;;   |for x in xs
          (cond
           ;; case 1 or 2
           ((save-excursion (swift-smie--backward-token)
                            (equal (swift-smie--backward-token) "in"))
            (goto-char (nth 1 parent))
            (swift-smie--backward-token) ; "in"
            (swift-smie--backward-token) ; "IMP;" or "x"
            (swift-smie--backward-sexps-until
             swift-smie--statement-parent-tokens)
            (when (equal (swift-smie--forward-token) "for")
              (swift-smie--backward-token)
              (current-column)))
           ;; case 3
           ((save-excursion (equal (swift-smie--forward-token) "in"))
            (goto-char (nth 1 parent))
            (swift-smie--backward-sexps-until
             swift-smie--statement-parent-tokens)
            (when (equal (swift-smie--forward-token) "for")
              (swift-smie--backward-token)
              (current-column)))
           ;; case 4
           ((save-excursion
              (and (equal (swift-smie--forward-token) "for")
                   (progn
                     (goto-char pos)
                     (equal
                      (nth 2 (swift-smie--backward-sexps-until '("IMP;" "in")))
                      "in"))))
            (current-column)))))))
    (when (not result-column)
      (goto-char pos))
    result-column))

(defun swift-smie--rule-for ()
  "Special rule for 'for' statement."
  (let ((column (swift-smie--backward-for-head)))
    (when column (cons 'column column))))

(defun swift-smie--op-parent-tokens (&optional ignore-question)
  "Return parent tokens for operators."
  (append swift-smie--expression-parent-tokens
          (list "=" (if ignore-question ":" "?") ":")))

(defun swift-smie--goto-op-parent (&optional ignore-question)
  "Go to the parent of the current operator token."
  (swift-smie--backward-sexps-until
   (swift-smie--op-parent-tokens ignore-question)))

(defun swift-smie--op-offset (parent &optional ignore-question offset)
  "Return the offset after operator tokens."
  (- (or offset swift-indent-multiline-statement-offset)
     ;; Special handling for assignment and conditional operators:
     ;;
     ;; let x = 1 +
     ;;         2 +
     ;;         3
     ;;
     ;; foo()
     ;;   ? bar1() *
     ;;     buz1()
     ;;   : bar2() *
     ;;     buz2()
     ;;
     ;; rather than:
     ;;
     ;; let x = 1 +
     ;;           2 +
     ;;           3
     ;;
     ;; foo()
     ;;   ? bar1() *
     ;;       buz1()
     ;;   : bar2() *
     ;;       buz2()
     ;;
     ;; The reason for subtracting hardcoded 2 rather than just returning 0 is
     ;; compatibility for older implementation that indented based on "=" token
     ;; rather than the first token of RHS.
     ;;
     ;; Old implementation:
     ;;
     ;; let x =      1 +
     ;;         2 // swift-indent-multiline-statement-offset from "=" token
     ;;
     ;; New implementation:
     ;;
     ;; let x =      1 +
     ;;              2 // swift-indent-multiline-statement-offset - 2
     ;;                // from the first token of RHS
     (if (member (nth 2 parent) (list "=" (if ignore-question ":" "?") ":"))
         2 0)))

(defun swift-smie--rule-after-op (&optional ignore-question offset)
  (swift-smie--op-offset (swift-smie--goto-op-parent) ignore-question offset))

(defun swift-smie--rule-before-op (&optional ignore-question offset)
  ;; When the token is at the beginning of the line (i.e. not called from
  ;; smie-indent-virtual) and if the previous line is indented manually,
  ;; aligns with it.
  ;; When the token is not at the beginning of the line, and if the current
  ;; line is indented manually, aligns with the first token.
  (let*
      ((pos (point))
       (bolp (smie-indent--bolp-1))
       (bol-of-previous-token
        (save-excursion
          (swift-smie--backward-token)
          (line-beginning-position)))
       (parent (swift-smie--goto-op-parent))
       (offset (swift-smie--op-offset parent ignore-question offset)))
    (if (< (point) bol-of-previous-token)
        (progn
          (goto-char bol-of-previous-token)
          (skip-chars-forward " \t")
          (cons 'column (- (current-column) (if bolp 0 offset))))
      (goto-char pos)
      (swift-smie--indent-keyword
       (swift-smie--op-parent-tokens ignore-question)
       (if bolp offset 0)))))


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
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?: ??))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Additional symbols
    (modify-syntax-entry ?_ "w" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)

    ;; Parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

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
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
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
