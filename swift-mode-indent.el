;;; swift-mode-indent.el --- Major-mode for Apple's Swift programming language, indentation. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;
;; Version: 2.1
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

;; Routines for Indentation

;;; Code:

(require 'swift-mode-lexer)

;;;###autoload
(defcustom swift-mode:basic-offset 4
  "Amount of indentation for block contents."
  :type 'integer
  :group 'swift
  :safe 'integerp)

;;;###autoload
(defcustom swift-mode:parenthesized-expression-offset 2
  "Amount of indentation inside parentheses and square brackets."
  :type 'integer
  :group 'swift
  :safe 'integerp)

;;;###autoload
(defcustom swift-mode:multiline-statement-offset 2
  "Amount of indentation for continuations of expressions."
  :type 'integer
  :group 'swift
  :safe 'integerp)

;;;###autoload
(defcustom swift-mode:switch-case-offset 0
  "Amount of indentation for case labels in switch statements."
  :type 'integer
  :group 'swift
  :safe 'integerp)

;;;###autoload
(defcustom swift-mode:insert-space-after-asterisk-in-comment t
  "Automatically insert a space after asterisk in comment if non-nil."
  :type 'boolean
  :group 'swift
  :safe 'booleanp)

;;;###autoload
(defcustom swift-mode:auto-close-multiline-comment t
  "If non-nil, `indent-new-comment-line' automatically close multiline comment."
  :type 'boolean
  :group 'swift
  :safe 'booleanp)

;;;###autoload
(defcustom swift-mode:fix-comment-close t
  "Fix \"* /\" in incomplete multiline comment to \"*/\" if non-nil."
  :type 'boolean
  :group 'swift
  :safe 'booleanp)

(defconst swift-mode:statement-parent-tokens
  '(implicit-\; \; case-: { \( \[ anonymous-function-parameter-in)
  "Parent tokens for statements.")

(defconst swift-mode:expression-parent-tokens
  (append swift-mode:statement-parent-tokens
          '(\, < "where" "if" "guard" "while"))
  "Parent tokens for expressions.")

(defun swift-mode:indent-line ()
  (let ((indent (save-excursion (swift-mode:calculate-indent)))
        (current-indent
         (save-excursion (back-to-indentation) (current-column))))
    (if (<= (current-column) current-indent)
        ;; The cursor is on the left margin. Moving to the new indent.
        (indent-line-to indent)
      ;; Keeps current relative position.
      (save-excursion (indent-line-to indent)))))

(defun swift-mode:calculate-indent ()
  (back-to-indentation)

  (if (nth 4 (syntax-ppss))
      ;; If the 4th element of `(syntax-ppss)' is non-nil, the cursor is on
      ;; the 2nd or following lines of a multiline comment, because:
      ;;
      ;; - The 4th element of `(syntax-ppss)' is nil on the comment starter.
      ;; - We have called `back-to-indentation`.
      (swift-mode:calculate-indent-of-multiline-comment)
    (swift-mode:calculate-indent-of-code)))

(defun swift-mode:calculate-indent-of-multiline-comment ()
  (back-to-indentation)
  (let ((comment-beginning-position (nth 8 (syntax-ppss))))
    (forward-line -1)
    (back-to-indentation)
    (if (<= (point) comment-beginning-position)
        ;; The cursor was on the 2nd line of the comment, so aligns with
        ;; the asterisk of the comment starter.
        (progn
          (goto-char comment-beginning-position)
          (forward-char)
          (current-column))
      ;; The cursor was on the 3rd or following lines of the comment, so aligns
      ;; with a non-empty preceding line.
      (if (eolp)
          ;; The cursor is on an empty line, so seeks a non-empty-line.
          (swift-mode:calculate-indent-of-multiline-comment)
        (current-column)))))

(defun swift-mode:calculate-indent-of-code ()
  (back-to-indentation)
  (let* ((previous-token (save-excursion (swift-mode:backward-token)))
         (previous-type (swift-mode:token:type previous-token))
         (previous-text (swift-mode:token:text previous-token))
         (next-token (save-excursion (swift-mode:forward-token)))
         (next-type (swift-mode:token:type next-token))
         (next-text (swift-mode:token:text next-token))
         (next-is-on-same-line
          (<= (swift-mode:token:end next-token) (line-end-position))))
    (cond
     ;; Beginning of the buffer
     ((eq previous-type 'outside-of-buffer)
      0)

     ;; Before } on the same line
     ((and next-is-on-same-line (eq next-type '}))
      (goto-char (swift-mode:token:end next-token))
      (backward-list)
      (swift-mode:calculate-indent-after-open-curly-brace 0))

     ;; Before ) or ] on the same line
     ((and next-is-on-same-line (memq next-type '(\) \])))
      (goto-char (swift-mode:token:end next-token))
      (backward-list)
      (swift-mode:calculate-indent-of-expression
       swift-mode:expression-parent-tokens
       0
       ;; Stops scanning at BOL:
       ;;
       ;; foo
       ;;   .bar(
       ;;     1
       ;;   )
       ;;
       ;; rather than
       ;;
       ;; foo
       ;;   .bar(
       ;;   1
       ;; )
       'any))

     ;; Before , on the same line
     ((and next-is-on-same-line (eq next-type '\,))
      (swift-mode:calculate-indent-of-prefix-comma))

     ;; After ,
     ((eq previous-type '\,)
      (goto-char (swift-mode:token:start previous-token))
      (swift-mode:calculate-indent-after-comma))

     ;; Before "in" on the same line
     ((and next-is-on-same-line (equal next-text "in"))
      ;; When it is for-in statement, align with the token after "for":
      ;;
      ;; for
      ;;   x
      ;;   in
      ;;   foo
      ;;
      ;; for x
      ;;     in
      ;;     foo
      ;;
      ;; When it is anonymous function, align with the token after {:
      ;;
      ;; foo {
      ;;   x
      ;;   in
      ;;   ...
      ;; }
      ;;
      ;;
      ;; foo { x
      ;;       in
      ;;  ...
      ;; }
      ;;
      ;; foo { [
      ;;         weak self
      ;;       ]
      ;;       (
      ;;         x,
      ;;         y
      ;;       )
      ;;       -> Void
      ;;       in
      ;;   a
      ;; }
      (swift-mode:calculate-indent-of-expression '("for" {)))

     ;; Before "case" or "default" on the same line, for switch statement
     ((and
       next-is-on-same-line
       (member next-text '("case" "default"))
       (save-excursion
         (equal (swift-mode:token:text
                 (swift-mode:backward-sexps-until
                  '("switch" "enum" "for" "while" "if" "guard")))
                "switch")))
      ;; "case" is used for "switch", "enum", "for", "while", "if", and "guard".
      ;; Only switch statement has special indentation rule.
      ;;
      ;; switch foo {
      ;; default:
      ;;   aaa
      ;; case A:
      ;;   aaa
      ;; case B, C, D:
      ;;   aaa
      ;; case E(1, 2, 3, (4, 5)) where aaa,
      ;;      F(1, 2, 3, (4, 5)) where aaa:
      ;;   aaa
      ;; case G: print(1); case H: print(2)
      ;; case I:
      ;;   ...
      ;; }
      ;;
      ;; enum Foo {
      ;;   case A
      ;;   case B, C, D
      ;;   indirect
      ;;     case E(x: Int, y: Int)
      ;; }
      ;;
      ;; enum Foo: Int, A, B {
      ;;   case A = 1, B = 2
      ;;   case C = 3
      ;; }
      ;;
      ;; for
      ;;   case let (x, y) in tuples {
      ;; }
      ;; if
      ;;   case let (x, y) = tuple {
      ;; }
      ;; while
      ;;   case let (x, y) = tuple {
      ;; }
      ;;
      ;; Searches sibling "case" at the beginning of a line. If found, aligns
      ;; with it.
      ;;
      ;; Otherwise, searches "switch" and aligh with it with offset.
      (let ((parent (swift-mode:backward-sexps-until
                     '("switch") nil '("case" "default"))))
        (if (equal (swift-mode:token:text parent) "switch")
            ;; Inside a switch-statement. Aligns with the "switch"
            (swift-mode:calculate-indent-of-expression
             swift-mode:statement-parent-tokens
             swift-mode:switch-case-offset)
          ;; Other cases. Aligns with the previous case.
          (swift-mode:align-with-current-line))))

     ;; Before "where" on the same line
     ((and next-is-on-same-line (equal next-text "where"))
      ;; switch {
      ;; case let P(x)
      ;;        where
      ;;          a,
      ;;      let Q(x)
      ;;        where
      ;;          a:
      ;;   aaa
      ;; }
      ;;
      ;; for case (x, y) in xys
      ;;            where
      ;;              aaa {
      ;; }
      ;;
      ;; do {
      ;; } catch let P(x)
      ;;           where
      ;;             aaa
      ;;
      ;; func foo<A: AAA,
      ;;          B: BBB
      ;;            where
      ;;              ABC>() {
      ;; }
      ;;
      ;; class Foo<A,
      ;;           B,
      ;;           C>: AAA,
      ;;               BBB,
      ;;               CCC
      ;;   where
      ;;     ABC {
      ;; }
      (let ((parent (save-excursion (swift-mode:backward-sexps-until
                                     (append swift-mode:statement-parent-tokens
                                             '("case"))))))
        (swift-mode:calculate-indent-of-expression
         (append swift-mode:statement-parent-tokens
                 '(< "case" "catch" "for")
                 (if (equal (swift-mode:token:text parent) "case") '(\,) '()))
         swift-mode:multiline-statement-offset)))

     ;; After {
     ((eq previous-type '{)
      (goto-char (swift-mode:token:start previous-token))
      (swift-mode:calculate-indent-after-open-curly-brace
       swift-mode:basic-offset))

     ;; After ( or [
     ((memq previous-type '(\( \[))
      (goto-char (swift-mode:token:start previous-token))
      (swift-mode:calculate-indent-of-expression
       swift-mode:expression-parent-tokens
       swift-mode:parenthesized-expression-offset
       ;; Stops scanning at BOL:
       ;;
       ;; foo
       ;;   .bar(
       ;;     1
       ;;   )
       ;;
       ;; rather than
       ;;
       ;; foo
       ;;   .bar(
       ;;   1
       ;; )
       'any
       nil
       swift-mode:parenthesized-expression-offset))

     ;; After "where"
     ((equal previous-text "where")
      ;; switch {
      ;; case let P(x) where
      ;;        A,
      ;;      let Q(x) where
      ;;        A:
      ;;   aaa
      ;; case let P(x)
      ;;        where
      ;;          a,
      ;;      let Q(x)
      ;;        where
      ;;          a:
      ;;   aaa
      ;; case let P(x), let Q(x) where
      ;;                  a
      ;; }
      ;;
      ;; for case let (x, y) in xys where
      ;;            aaa {
      ;; }
      ;;
      ;; for case let (x, y) in xys
      ;;            where
      ;;              aaa {
      ;; }
      ;;
      ;; do {
      ;; } catch let P(x) where
      ;;           aaa
      ;; do {
      ;; } catch let P(x)
      ;;           where
      ;;             aaa
      ;;
      ;;
      ;;
      ;; func foo<A: AAA,
      ;;          B: BBB where
      ;;            ABC>() {
      ;; }
      ;;
      ;; func foo<A: AAA,
      ;;          B: BBB
      ;;            where
      ;;              ABC>() {
      ;; }
      ;;
      ;; class Foo<A,
      ;;           B,
      ;;           C> A,
      ;;              B,
      ;;              C where
      ;;   ABC {
      ;; }
      ;;
      ;; class Foo<A,
      ;;           B,
      ;;           C>: A,
      ;;               B,
      ;;               C
      ;;   where
      ;;     ABC {
      ;; }
      (goto-char (swift-mode:token:start previous-token))
      (if (swift-mode:bol-other-than-comments-p)
          (swift-mode:align-with-current-line
           swift-mode:multiline-statement-offset)
        (let ((parent (save-excursion
                        (swift-mode:backward-sexps-until
                         (append swift-mode:statement-parent-tokens
                                 '("case"))))))
          (swift-mode:calculate-indent-of-expression
           (append swift-mode:statement-parent-tokens
                   '(< "case" "catch" "for")
                   (if (equal (swift-mode:token:text parent) "case") '(\,) '()))
           swift-mode:multiline-statement-offset))))

     ;; After implicit-\; or ;
     ((memq previous-type '(implicit-\; \;))
      (goto-char (swift-mode:token:start previous-token))
      (swift-mode:calculate-indent-of-expression
       (remove '\; (remove 'implicit-\; swift-mode:statement-parent-tokens))
       0
       '(implicit-\; \;)))

     ;; After "in" for anonymous function parameters
     ((eq previous-type 'anonymous-function-parameter-in)
      (goto-char (swift-mode:token:start previous-token))
      (swift-mode:backward-sexps-until '({))
      (swift-mode:calculate-indent-after-open-curly-brace
       swift-mode:basic-offset))

     ;; After "in" for "for" statements
     ((equal previous-text "in")
      ;; Aligns with "in" if it is at the start of the line:
      ;;
      ;; for
      ;;   x
      ;;   in
      ;;   foo
      ;;
      ;; for x
      ;;     in
      ;;     foo
      ;;
      ;; Otherwise, aligns with the next token of the "for".
      ;;
      ;; for x in
      ;;     foo
      ;;
      ;; for
      ;;   x in
      ;;   foo
      (goto-char (swift-mode:token:start previous-token))
      (if (swift-mode:bol-other-than-comments-p)
          (swift-mode:align-with-current-line)
        (let ((parent (swift-mode:backward-sexps-until '("for" {))))
          (swift-mode:align-with-next-token parent))))

     ;; After case ... : or default:
     ((eq previous-type 'case-:)
      (goto-char (swift-mode:token:start previous-token))
      (swift-mode:calculate-indent-of-expression
       swift-mode:statement-parent-tokens
       swift-mode:basic-offset))

     ;; Before ; on the same line
     ((and next-is-on-same-line (eq next-type '\;))
      (swift-mode:calculate-indent-of-expression
       (remove '\; (remove 'implicit-\; swift-mode:statement-parent-tokens))
       0
       '(implicit-\; \;)))

     ;; After if, guard, while
     ((member previous-text '("if" "guard" "while"))
      (swift-mode:calculate-indent-of-expression
       swift-mode:statement-parent-tokens
       swift-mode:multiline-statement-offset))

     ;; After attributes at the beginning of a statement, without arguments
     ((and
       (string-prefix-p "@" previous-text)
       (memq (save-excursion
               (goto-char (swift-mode:token:start previous-token))
               (swift-mode:token:type (swift-mode:backward-token)))
             swift-mode:statement-parent-tokens))
      ;; Aligns with the attribute.
      (goto-char (swift-mode:token:start previous-token))
      (swift-mode:align-with-next-token (swift-mode:backward-token)))

     ;; After attributes at the beginning of a statement, with arguments
     ((and
       (eq previous-type '\))
       (save-excursion
         (backward-list)
         (and
          (string-prefix-p
           "@"
           (swift-mode:token:text (swift-mode:backward-token)))
          (memq  (swift-mode:token:type (swift-mode:backward-token))
                 swift-mode:statement-parent-tokens))))
      (backward-list)
      (swift-mode:backward-token)
      (swift-mode:align-with-next-token (swift-mode:backward-token)))

     ;; Otherwise, it is continuation of the previous line
     (t
      (goto-char (swift-mode:token:end previous-token))
      (swift-mode:backward-token-or-list)
      (swift-mode:calculate-indent-of-expression
       swift-mode:expression-parent-tokens
       swift-mode:multiline-statement-offset
       'any)))))

(defun swift-mode:calculate-indent-of-expression
    (parents
     &optional
     offset
     stop-at-eol-token-types
     stop-at-bol-token-types
     bol-offset)
  "Return start column of the current expressions or statement plus OFFSET.

If OFFSET is omitted, it is assumed to be 0.
PARENTS is a list of token types that precedes an expression or a statement.
See `swift-mode:backward-sexps-until' for the details of
STOP-AT-EOL-TOKEN-TYPES and STOP-AT-BOL-TOKEN-TYPES.
If scanning stops at STOP-AT-EOL-TOKEN-TYPES, align with the next token with
BOL-OFFSET.
If scanning stops at STOP-AT-BOL-TOKEN-TYPES, align with that token with
BOL-OFFSET.
If STOP-AT-BOL-TOKEN-TYPES is the symbol `any', the cursor is assumed to be
on the previous line."
  (save-excursion
    (let* ((parent (swift-mode:backward-sexps-until
                    parents
                    stop-at-eol-token-types
                    stop-at-bol-token-types))
           (parent-end (swift-mode:token:end parent))
           (stopped-at-parent
            (or (memq (swift-mode:token:type parent) parents)
                (member (swift-mode:token:text parent) parents)
                (eq (swift-mode:token:type parent) 'outside-of-buffer)))
           (stopped-at-eol
            (and
             (not stopped-at-parent)
             stop-at-eol-token-types
             (or
              (eq stop-at-eol-token-types 'any)
              (memq (swift-mode:token:type parent)
                    stop-at-eol-token-types)
              (member (swift-mode:token:text parent)
                      stop-at-eol-token-types)))))
      (when (or stopped-at-parent stopped-at-eol)
        (goto-char parent-end)
        (forward-comment (point-max)))
      ;; Now, the cursor is at the first token of the expression.

      (if stopped-at-parent
          ;; The cursor is at the start of the entire expression.
          ;; Aligns with the start of the expression with offset.
          (swift-mode:align-with-next-token parent offset)
        ;; The cursor is at the middle of the expression.
        ;; Aligns with this line with bol-offset.
        (swift-mode:align-with-current-line bol-offset)))))

(defun swift-mode:calculate-indent-after-open-curly-brace (offset)
  "Return indentation after open curly braces.

Assuming the cursor is on the open parenthesis.
OFFSET is the offset of the contents.
This function is also used for close-curly-brace."
  ;; If the statement is multiline expression, aligns with the start of
  ;; the line on which the open brace is:
  ;;
  ;; foo()
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;;   }
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;;   }
  ;;
  ;; rather than
  ;;
  ;; foo()
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;; }
  ;;   .then { x in
  ;;     foo()
  ;;     foo()
  ;; }
  ;;
  ;; Otherwise, aligns with the start of the whole statement:
  ;;
  ;; for x in
  ;;     xs
  ;;       .foo() {
  ;; }
  ;;
  ;; rather than
  ;;
  ;; for x in
  ;;     xs
  ;;       .foo() {
  ;;       }
  ;;
  ;; Note that curly brace after binary operator is a part of
  ;; a multiline expression:
  ;;
  ;; for x in
  ;;     xs
  ;;     +++ { x in
  ;;       // this is not the body of the for-statement.
  ;;     } {
  ;;   // The body of the for-statement.
  ;; }
  (let ((pos (point))
        next-token
        is-declaration-or-control-statement-body)
    (if (save-excursion
          (eq (swift-mode:token:type (swift-mode:backward-token))
              'binary-operator))
        ;; for x in
        ;;     xs
        ;;     +++ { x in
        ;;       // this is not the body of the for statement.
        ;;     } {
        ;;   ...
        ;; }
        (setq is-declaration-or-control-statement-body nil)
      (save-excursion
        (swift-mode:backward-sexps-until swift-mode:statement-parent-tokens)
        (swift-mode:forward-token)
        (setq next-token (swift-mode:forward-token-or-list))
        (while (<= (point) pos)
          (cond
           ((member
             (swift-mode:token:text next-token)
             '("for" "while" "repeat" "if" "else" "defer" "do" "catch"
               "get" "set" "willSet" "didSet" "func" "init" "subscript"
               "enum" "struct" "class" "extension" "prefix" "postfix" "infix"
               "precedencegroup"))
            (setq is-declaration-or-control-statement-body t)
            (goto-char (1+ pos)))

           ((and
             (equal (swift-mode:token:text next-token) "protocol")
             (not (equal (swift-mode:token:text
                          (save-excursion (swift-mode:forward-token)))
                         "<")))
            (setq is-declaration-or-control-statement-body t)
            (goto-char (1+ pos)))

           ((equal (swift-mode:token:text next-token) "var")
            ;; There are several cases:
            ;;
            ;; var foo = bar
            ;;   .then { x in
            ;;       x
            ;;   }
            ;;   .then { x in
            ;;       x
            ;;   }
            ;;
            ;; var foo = bar
            ;;   .baz {
            ;;     willSet {
            ;;         ...
            ;;     }
            ;; }
            ;;
            ;; var foo {
            ;;     get {
            ;;         ...
            ;;     }
            ;; }
            ;;
            ;; Note that the 1st and the 2nd cases cannot be distinguished
            ;; without inspecting the contents of the block.
            ;; We indent the 2nd case like the 1st case for now.
            ;; Future implementation may use more sophisticated logic.
            (goto-char pos)
            (setq is-declaration-or-control-statement-body
                  (equal (swift-mode:token:text
                          (swift-mode:backward-sexps-until '("var" "=")))
                         "var"))
            (goto-char (1+ pos)))

           (t
            ;; Suppose indenting the A token below.
            ;;
            ;; foo {
            ;;   A
            ;;
            ;; This function is called on the open curly brace.
            ;; If the close curly brace doesn't exist,
            ;; swift-mode:forward-token-or-list results in
            ;; "Unbalanced parentheses" error.
            ;; So if the point is just before the open curly brace,
            ;; exits immediately.
            (forward-comment (point-max))
            (if (< (point) pos)
                (setq next-token (swift-mode:forward-token-or-list))
              (goto-char (1+ pos))))))))
    (swift-mode:calculate-indent-of-expression
     swift-mode:statement-parent-tokens
     offset
     (if is-declaration-or-control-statement-body nil 'any)
     nil
     offset)))

(defun swift-mode:calculate-indent-of-prefix-comma ()
  "Return indentation for prefix comma.

Example:

let x = [ 1
        , 2
        , 3
]

class Foo: A
         , B
         , C

case A
   , B
   , C
       :

var x = 1
  , y = 2
  , z = 3

This is also known as Utrecht-style in the Haskell community."
  (let* ((comma-type-and-statement-parent (swift-mode:detect-type-of-comma))
         (comma-type (nth 0 comma-type-and-statement-parent))
         (statement-parent (nth 1 comma-type-and-statement-parent)))
    (if (eq comma-type 'condition-list)
        (swift-mode:calculate-indent-of-prefix-comma-of-condition-list
         statement-parent)
      (let* ((parents (swift-mode:parents-of-comma comma-type))
             (parent (swift-mode:backward-sexps-until parents
                                                      nil
                                                      '(\,)))
             (parent-end (swift-mode:token:end parent))
             (stopped-at-parent
              (or (memq (swift-mode:token:type parent) parents)
                  (member (swift-mode:token:text parent) parents)
                  (eq (swift-mode:token:type parent) 'outside-of-buffer))))
        (if stopped-at-parent
            (progn
              ;; Aligns with the end of the parent.
              (goto-char parent-end)
              (backward-char)
              (current-column))
          ;; Aligns with the previous comma.
          (swift-mode:align-with-current-line))))))

(defun swift-mode:calculate-indent-of-prefix-comma-of-condition-list
    (statement-parent)
  ;; The comma is in a condition-list but not in an enum-case-pattern-list.
  ;;
  ;; Example:
  ;;
  ;; if case let P(x)
  ;;       , let Q(x)     // comma for enum-case-pattern-list
  ;;       , let R(x) = a // comma for enum-case-pattern-list
  ;;  , let x = x         // comma for condition-list
  ;;  , foo == bar        // comma for condition-list
  ;;
  ;; We scan from beginning of the statement and remembers last anchor token,
  ;; i.e. "if", "guard", "while", or comma at the beginning of the line.
  (let ((pos (point))
        next-token
        (anchor statement-parent)
        in-case-pattern-list)
    (goto-char (swift-mode:token:end statement-parent))
    (setq next-token (swift-mode:forward-token-or-list))
    (while (< (point) pos)
      (cond
       ((equal (swift-mode:token:text next-token) "case")
        (setq in-case-pattern-list t))

       ((equal (swift-mode:token:text next-token) "=")
        (setq in-case-pattern-list nil))

       ((member (swift-mode:token:text next-token) '("if" "guard" "while"))
        (setq anchor next-token))

       ((and (not in-case-pattern-list)
             (eq (swift-mode:token:type next-token) '\,)
             (save-excursion
               (goto-char (swift-mode:token:start next-token))
               (swift-mode:bol-other-than-comments-p)))
        (setq anchor next-token)))
      (setq next-token (swift-mode:forward-token-or-list)))
    (if (eq (swift-mode:token:type anchor) '\,)
        ;; Aligns with the previous comma.
        (progn
          (goto-char (swift-mode:token:start anchor))
          (swift-mode:align-with-current-line))
      ;; Aligns with the end of the anchor
      (goto-char (swift-mode:token:end anchor))
      (backward-char)
      (current-column))))

(defun swift-mode:calculate-indent-after-comma ()
  "Return indentation after comma.

Assuming the cursor is on the comma."
  (let* ((comma-type-and-statement-parent (swift-mode:detect-type-of-comma))
         (comma-type (nth 0 comma-type-and-statement-parent))
         (statement-parent (nth 1 comma-type-and-statement-parent)))
    (if (eq comma-type 'condition-list)
        (swift-mode:calculate-indent-after-comma-of-condition-list
         statement-parent)
      (swift-mode:align-with-next-token
       (swift-mode:backward-sexps-until
        (swift-mode:parents-of-comma comma-type)
        '(\,))))))

(defun swift-mode:calculate-indent-after-comma-of-condition-list
    (statement-parent)
  ;; The comma is in a condition-list but not in an enum-case-pattern-list.
  ;;
  ;; Example:
  ;;
  ;; if
  ;;   case let P(x),     // comma for enum-case-pattern-list
  ;;        let Q(x),     // comma for enum-case-pattern-list
  ;;        let R(x) = a, // comma for condition-list
  ;;   let x = x,         // comma for condition-list
  ;;   foo == bar
  ;;
  ;; We scan from beginning of the statement and remembers last parent token,
  ;; i.e. "if", "guard", "while", or comma at the end of the line.
  (let ((pos (point))
        next-token
        (parent statement-parent)
        in-case-pattern-list)
    (goto-char (swift-mode:token:end statement-parent))
    (setq next-token (swift-mode:forward-token-or-list))
    (while (< (point) pos)
      (cond
       ((equal (swift-mode:token:text next-token) "case")
        (setq in-case-pattern-list t))

       ((equal (swift-mode:token:text next-token) "=")
        (setq in-case-pattern-list nil))

       ((member (swift-mode:token:text next-token) '("if" "guard" "while"))
        (setq parent next-token))

       ((and (not in-case-pattern-list)
             (eq (swift-mode:token:type next-token) '\,)
             (swift-mode:eol-other-than-comments-p))
        (setq parent next-token)))
      (setq next-token (swift-mode:forward-token-or-list)))
    (swift-mode:align-with-next-token parent)))

(defun swift-mode:detect-type-of-comma ()
  "Return type of comma token under the cursor.

Comma type is a list where:
0th element is one of the following:

- tuple-or-array (inside () or [])
- type-parameters-or-requirements (inside <>)
- enum-case-pattern-list (e.g. if case P, Q, R = x)
- condition-list (e.g. if let x = x, let y = y)
- variable-declarations (e.g. let x = 1, y = 2)
- switch-case-or-enum-case-item-list (e.g. switch {case P, Q, R: a} or
  enum {case A, B, C})
- class-like-declarations (supertypes of class, or where clause after
  super types)

1st element is the token before the beginning of the statement.
"
  ;; Various examples:
  ;;
  ;; let x = ( // simple case
  ;;   1,
  ;;   2,
  ;;   3
  ;; )
  ;;
  ;; let x = [ // simple case
  ;;   1,
  ;;   2,
  ;;   3
  ;; ]
  ;;
  ;; let x: Foo<A, B> = a, // "let" is not a part of elements
  ;;     y: Foo<A, B> = b,
  ;;     z: Foo<A, B> = c
  ;;
  ;; switch foo {
  ;; case (let x, // "let" is a part of an element
  ;;       Y,
  ;;       Z):
  ;;   aaa
  ;; }
  ;;
  ;; class Foo<A where A: B, // "where" is not a part of elements
  ;;                   A: C,
  ;;                   A: D> {
  ;; }
  ;;
  ;; switch foo {
  ;; case A(x) where p(x), // "case" is not a part of elements
  ;;      B(x) where p(x), // "where" is a part of an element
  ;;      C(x) where p(x):
  ;;   aaa
  ;; }
  ;;
  ;; if // or guard or while
  ;;   let x = x, // "let" is a part of an element
  ;;   let y = y,
  ;;   let z = z,
  ;;   case P(a, b, c), // "case" is a part of an element of condition-list
  ;;        Q(a, b, c) = abc, // "case" is not a part of elements of
  ;;                          // enum-case-pattern-list
  ;;   case (a, b, c) = abc,
  ;;   aaa {
  ;;
  ;;   bbb
  ;; }
  ;;
  ;; See also SE-0099 and SE-0043:
  ;; https://github.com/apple/swift-evolution/blob/master/proposals/0099-conditionclauses.md
  ;; https://github.com/apple/swift-evolution/blob/master/proposals/0043-declare-variables-in-case-labels-with-multiple-patterns.md
  ;;
  ;; class Foo<T>: A,
  ;;               B,
  ;;               C
  ;;   where
  ;;     T: A,
  ;;     T: B,
  ;;     T: C {
  ;; }
  ;;
  ;; extension _ArrayType
  ;;   where
  ;;     Generator.Element: A,
  ;;     Generator.Element: B,
  ;;     Generator.Element: C {
  ;; }
  ;;
  ;; func foo<T> -> Int
  ;;   where
  ;;     T: A,
  ;;     T: B,
  ;;     T: C {
  ;; }
  ;;
  ;; enum Foo {
  ;;   case A(x: Int),
  ;;        B(y: Int),
  ;;        C(z: Int)
  ;;   case D(x: Int)
  ;;      , E(y: Int)
  ;;      , F(z: Int)
  ;; }
  ;;
  ;; enum Foo: Int {
  ;;   case A,
  ;;        B,
  ;;        C = 2
  ;;   case D = 3
  ;;      , E
  ;;      , F
  ;; }
  (save-excursion
    (let ((pos (point))
          (parent (swift-mode:backward-sexps-until
                   (cons '< swift-mode:statement-parent-tokens))))
      (cond
       ((memq (swift-mode:token:type parent) '(\( \[))
        (list 'tuple-or-array parent))

       ((eq (swift-mode:token:type parent) '<)
        (list 'type-parameters-or-requirements parent))

       (t
        (goto-char (swift-mode:token:end parent))
        (let ((next-token (swift-mode:forward-token-or-list))
              result)
          (while (and (<= (point) pos) (not result))
            (cond
             ((member (swift-mode:token:text next-token)
                      '("if" "guard" "while"))
              ;; Conditions
              ;;
              ;; Distinguishes condition-list and enum-case-pattern-list:
              ;;
              ;; if
              ;;   let x = x,
              ;;   case P(a, b, c),
              ;;        Q(a, b, c),
              ;;        R(a, b, c) = abc,
              ;;   let x = x,
              ;;   foo == bar,
              ;;   case (a, b, c) = abc {
              ;; }
              (goto-char pos)
              (if (equal
                   (swift-mode:token:text (swift-mode:backward-sexps-until
                                           '("if" "guard" "while" "case" "=")))
                   "case")
                  (setq result 'enum-case-pattern-list)
                (setq result 'condition-list)))

             ((member (swift-mode:token:text next-token)
                      '("let" "var"))
              (setq result 'variable-declarations))

             ((equal (swift-mode:token:text next-token)
                     "case")
              (setq result 'switch-case-or-enum-case-item-list))

             ((equal (swift-mode:token:text next-token)
                     "where")
              (setq result 'type-parameters-or-requirements))

             ((eq (swift-mode:token:type next-token) 'typing-:)
              (setq result 'class-like-declarations)))

            (setq next-token (swift-mode:forward-token-or-list)))
          (if (and (> (point) pos) (eq (swift-mode:token:type next-token) '<>))
              ;; The comma was inside <> but scanner misunderstand < as
              ;; a binary-operator.
              (list 'type-parameters-or-requirements parent)
            (list result parent))))))))

(defun swift-mode:parents-of-comma (comma-type)
  "Return parent token types of comma token Ffrom COMMA-TYPE."
  (append
   swift-mode:statement-parent-tokens
   (cond
    ((eq comma-type 'tuple-or-array)
     '(\( \[))

    ((eq comma-type 'type-parameters-or-requirements)
     '(< "where"))

    ((eq comma-type 'enum-case-pattern-list)
     '("case"))

    ((eq comma-type 'variable-declarations)
     '("let" "var"))

    ((eq comma-type 'switch-case-or-enum-case-item-list)
     '("case"))

    ((eq comma-type 'class-like-declarations)
     '(typing-: "where")))))

(defun swift-mode:backward-sexps-until (token-types
                                        &optional
                                        stop-at-eol-token-types
                                        stop-at-bol-token-types)
  "Backward sexps until a token with one of given token types appears.
Return the token.
When this function returns, the cursor is at the start of the token.

TOKEN-TYPES is a list of guard token typess.  This function backs to a token
with  one of those token types.
STOP-AT-EOL-TOKEN-TYPES is a list of token types that if we skipped the end of
a line just after a token with one of given toke typen, the function returns.
Typically, this is a list of token types that separates list elements
\(e.g. ',', ';'). If STOP-AT-EOL-TOKEN-TYPES is the symbol `any', it matches
all tokens.
STOP-AT-BOL-TOKEN-TYPES is a list of token types that if we hit
the beginning of a line just before a token with one of given token types,
the function returns. Typically, this is a list of token types that starts
list element (e.g. 'case' of switch statement body). If STOP-AT-BOL-TOKEN-TYPES
is the symbol `any', it matches all tokens."
  (let*
      ((parent (swift-mode:backward-token-or-list))
       (type (swift-mode:token:type parent))
       (text (swift-mode:token:text parent)))
    (while (not
            ;; Stops loop when...
            (or
             ;; Hits a guard token.
             (member type token-types)
             (member text token-types)

             ;; Beginning of the buffer.
             (eq type 'outside-of-buffer)

             ;; When this function is called on "," token before position (1),
             ;; this function stops just before the "," token after "Foo".
             ;;
             ;; case Foo,
             ;;      Bar, Baz, // (1)
             ;;      AAA
             (and stop-at-eol-token-types
                  (save-excursion
                    (swift-mode:forward-token-or-list)
                    (forward-comment (- (point)))
                    (swift-mode:eol-other-than-comments-p))
                  (or (eq stop-at-eol-token-types 'any)
                      (member type stop-at-eol-token-types)
                      (member text stop-at-eol-token-types)))

             ;; When this function is called on "case" token before position
             ;; (1), this function stops just before "case Bar".
             ;;
             ;; switch foo {
             ;; case Foo:
             ;;     ...
             ;; case Bar: case Baz:
             ;;     ...
             ;; case AAA: // (1)
             ;; }
             (and stop-at-bol-token-types
                  (and
                   (or
                    (eq stop-at-bol-token-types 'any)
                    (member type stop-at-bol-token-types)
                    (member text stop-at-bol-token-types))
                   (swift-mode:bol-other-than-comments-p)))))
      (setq parent (swift-mode:backward-token-or-list))
      (setq type (swift-mode:token:type parent))
      (setq text (swift-mode:token:text parent)))
    parent))

(defun swift-mode:align-with-next-token (parent &optional offset)
  (let ((parent-end (swift-mode:token:end parent)))
    (goto-char parent-end)
    (forward-comment (point-max))
    (swift-mode:goto-non-comment-bol)
    (when (< (point) parent-end)
      (goto-char parent-end))
    (swift-mode:skip-whitespaces)
    (+ (or offset 0) (current-column))))

(defun swift-mode:align-with-current-line (&optional offset)
  (swift-mode:goto-non-comment-bol)
  (swift-mode:skip-whitespaces)
  (+ (or offset 0) (current-column)))

(defun swift-mode:backward-token-or-list ()
  "Move point to the start position of the previous token or list.
Return the token skipped."
  (let* ((previous-token (swift-mode:backward-token))
         (previous-type (swift-mode:token:type previous-token))
         (previous-text (swift-mode:token:text previous-token))
         (previous-start (swift-mode:token:start previous-token))
         (previous-end (swift-mode:token:end previous-token)))
    (cond
     ;; List
     ((memq previous-type '(} \) \]))
      (goto-char previous-end)
      (backward-list)
      (swift-mode:token
       (cdr (assoc previous-type '((} . {})
                                   (\) . \(\))
                                   (\] . \[\]))))
       (buffer-substring-no-properties (point) previous-end)
       (point)
       previous-end))

     ;; Generic parameter list
     ((equal previous-text ">")
      (swift-mode:try-backward-generic-parameters)
      (if (= (point) previous-start)
          previous-token
        (swift-mode:token
         '<>
         (buffer-substring-no-properties (point) previous-end)
         (point)
         previous-end)))

     ;; Other token
     (t previous-token))))

(defun swift-mode:forward-token-or-list ()
  "Move point to the end position of the next token or list.
Return the token skipped."
  (let* ((next-token (swift-mode:forward-token))
         (next-type (swift-mode:token:type next-token))
         (next-text (swift-mode:token:text next-token))
         (next-start (swift-mode:token:start next-token))
         (next-end (swift-mode:token:end next-token)))
    (cond
     ;; List
     ((memq next-type '({ \( \[))
      (goto-char next-start)
      (forward-list)
      (swift-mode:token
       (cdr (assoc next-type '(({ . {})
                               (\( . \(\))
                               (\[ . \[\]))))
       (buffer-substring-no-properties next-start (point))
       next-start
       (point)))

     ;; Generic parameter list
     ((equal next-text "<")
      (swift-mode:try-forward-generic-parameters)
      (if (= (point) next-end)
          next-token
        (swift-mode:token
         '<>
         (buffer-substring-no-properties next-start (point))
         next-start
         (point))))

     ;; Other token
     (t next-token))))

(defun swift-mode:try-backward-generic-parameters ()
  "Move point to the start of the generic parameter list.

Keep position if the cursor is not at the end of a generic parameter list.

Assuming the cursor is on the close angle bracket.

It is a Generic parameter list if:
- it has matching angle brackets, and
- it does not have tokens that cannot appears in a generic parameter list."
  (swift-mode:try-skip-generic-parameters
   #'swift-mode:backward-token-or-list
   "<" ">"))

(defun swift-mode:try-forward-generic-parameters ()
  "Move point to the end of the generic parameter list.

Keep position if the cursor is not at the start of a generic parameter list.

Assuming the cursor is after the open angle bracket.

It is a Generic parameter list if:
- it has matching angle brackets, and
- it does not have tokens that cannot appears in a generic parameter list."
  (swift-mode:try-skip-generic-parameters
   #'swift-mode:forward-token-or-list
   ">" "<"))

(defconst siwft-mode:tokens-not-in-generic-parameter-list
  ;; Whitelisting tend to be fragile. So we list tokens that are
  ;; unlikely to appear in generic parameter lists in the current
  ;; version and future ones.
  ;;
  ;; Example of complex generic parameters:
  ;; <
  ;;   A: B,
  ;;   C: protocol<X, Y>
  ;;   where
  ;;     A: @aaa(1 + 2 + 3) D<Int>!,
  ;;     C == (@aaa(1) inout [E.F]?, G...) throws -> [Int:Int]
  ;; >
  ;;
  ;; We don't need to consider the contents of inner brackets because they are
  ;; skipped by `swift-mode:backward-token-or-list'.
  ;;
  ;; String literals, implicit parameter names, and numbers are also excluded
  ;; by `swift-mode:try-skip-generic-parameters'.
  '(outside-of-buffer
    \;
    { } \( \) \[ \]
    "true" "false"
    "class" "struct" "enum" "extension" "func" "operator"
    "try" "try?" "try!"
    "as" "as?" "as!"
    "is"
    "in"
    "init" "deinit" "get" "set" "willSet" "didSet" "subscript"
    "for" "case" "default" "while" "let" "var" "repeat" "if" "else"
    "guard" "break" "continue" "fallthrough" "return" "throw" "defer"
    "do" "catch" "import" "typealias" "associatedtype"))

(defun swift-mode:try-skip-generic-parameters
    (skip-token-or-list-function matching-bracket-text unmatching-bracket-text)
  (let ((pos (point))
        (prohibited-tokens (append
                            unmatching-bracket-text
                            siwft-mode:tokens-not-in-generic-parameter-list))
        (next-token (funcall skip-token-or-list-function)))
    (while
        (cond
         ((or (memq (swift-mode:token:type next-token) prohibited-tokens)
              (member (swift-mode:token:text next-token) prohibited-tokens)
              (string-match-p "^[\"$0-9]"
                              (swift-mode:token:text next-token)))
          ;; Not a generic parameter list. Returns to the initial position and
          ;; stops the loop.
          (goto-char pos)
          nil)

         ((equal (swift-mode:token:text next-token) matching-bracket-text)
          ;; Found the matching open angle bracket. Stops the loop.
          nil)

         ;; Otherwise, keep scanning
         (t t))
      (setq next-token (funcall skip-token-or-list-function)))
    next-token))

(defun swift-mode:bol-other-than-comments-p ()
  "Return t if there is nothing other than comments in the front of this line.

Return nil otherwise.
Newlines inside comments are ignored."
  ;; Foo // ← bol
  ;; /* */ Foo // ← bol
  ;; X /* */ Foo // ← not bol
  ;;
  ;; /*
  ;; */ /* */ /*
  ;; */ Foo // ← bol
  ;;
  ;; X /*
  ;; */ /* */ /*
  ;; */ Foo // ← not bol
  ;;
  ;; X
  ;; /* */ /*
  ;; */ Foo // ← bol
  (save-excursion
    (let ((pos (point)))
      (swift-mode:goto-non-comment-bol)
      (forward-comment (point-max))
      (<= pos (point)))))

(defun swift-mode:eol-other-than-comments-p ()
  "Return t if there is nothing other than comments until the end of this line.

Return nil otherwise.
Newlines inside comments are ignored."
  (save-excursion
    (let ((pos (point)))
      (swift-mode:goto-non-comment-eol)
      (forward-comment (- (point)))
      (<= (point) pos))))

(defun swift-mode:bolp ()
  "Return t if there is nothing in the front of this line.

Return nil otherwise."
  (save-excursion
    (skip-syntax-backward " ")
    (bolp)))

(defun swift-mode:skip-whitespaces ()
  "Skips forward whitespaces and newlines."
  (skip-syntax-forward " >"))

(defun swift-mode:incomplete-comment-p ()
  (and (nth 4 (syntax-ppss))
       (save-excursion
         (goto-char (nth 8 (syntax-ppss)))
         (not (forward-comment 1)))))

(defun swift-mode:indent-new-comment-line (&optional soft)
  "Break the line at the point and indent the new line.

If the point is inside a comment, continue the comment. If the comment is a
multiline comment, close the previous comment and start new one if
`comment-multi-line' is nil."
  (interactive)
  (let* ((parser-state (syntax-ppss))
         (is-inside-comment (nth 4 parser-state))
         (is-single-line-comment (eq (nth 7 parser-state) 1))
         (comment-beginning-position (nth 8 parser-state))
         (space-after-asterisk
          (if swift-mode:insert-space-after-asterisk-in-comment " " "")))
    (delete-horizontal-space)
    (if soft (insert-and-inherit ?\n) (newline 1))
    (delete-horizontal-space)

    (when is-inside-comment
      (insert-before-markers-and-inherit
       (cond
        (is-single-line-comment
         "// ")

        (comment-multi-line
         (save-excursion
           (beginning-of-line)
           (forward-char -1)
           (beginning-of-line)
           (if (<= (point) comment-beginning-position)
               ;; The cursor was on the 2nd line of the comment.
               ;; Uses default prefix.
               (concat "*" space-after-asterisk)
             ;; The cursor was on the 3nd or following lines of
             ;; the comment.
             ;; Use the prefix of the previous line.
             (back-to-indentation)
             (if (looking-at "\\*+")
                 (concat (match-string-no-properties 0) space-after-asterisk)
               ""))))

        (t
         (backward-char)
         (insert-before-markers-and-inherit " */")
         (forward-char)
         (save-excursion
           (goto-char comment-beginning-position)
           (looking-at "/\\*+")
           (concat (match-string-no-properties 0) space-after-asterisk))))))
    (indent-according-to-mode)

    ;; Closes incomplete multiline comment.
    (when (and swift-mode:auto-close-multiline-comment
               (not is-single-line-comment)
               (swift-mode:incomplete-comment-p))
      (save-excursion
        (end-of-line)
        (if soft (insert-and-inherit ?\n) (newline 1))
        (insert-before-markers-and-inherit "*/")
        (indent-according-to-mode)))))

(defun swift-mode:post-self-insert ()
  (cond
   ((and
     (= last-command-event ?*)
     (nth 4 (syntax-ppss))
     (save-excursion (backward-char) (skip-syntax-backward " ") (bolp)))
    (when swift-mode:insert-space-after-asterisk-in-comment
      (insert-before-markers-and-inherit " "))
    (indent-according-to-mode))
   ((and
     (= last-command-event ?/)
     swift-mode:fix-comment-close
     (nth 4 (syntax-ppss))
     (save-excursion
       (let ((pos (point)))
         (beginning-of-line)
         (and
          (looking-at "^[ 	]*\\*[ 	]+/")
          (eq (match-end 0) pos)
          (swift-mode:incomplete-comment-p)))))
    (backward-char)
    (delete-horizontal-space)
    (forward-char))))

(provide 'swift-mode-indent)

;;; swift-mode-indent.el ends here
