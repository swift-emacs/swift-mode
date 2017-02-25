;;; swift-mode-lexer.el --- Major-mode for Apple's Swift programming language, lexer. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;
;; Version: 2.2.4
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

;; Routines for Swift tokens

;; Token is a tuple consists of:
;;
;; - Token type
;; - Token text
;; - Start position (inclusive)
;; - End position (exclusive)

;;; Code:

(declare-function swift-mode:backward-sexps-until "swift-mode-indent.el"
                  (token-types
                   &optional
                   stop-at-eol-token-types
                   stop-at-bol-token-types))

(defun swift-mode:token (type text start end)
  "Construct and return a token.

TYPE is the type of the token such as `inix-operator' or {.
TEXT is the text of the token.
START is the start position of the token.
END is the point after the token."
  (list type text start end))

(defun swift-mode:token:type (token)
  "Return the type of TOKEN."
  (nth 0 token))

(defun swift-mode:token:text (token)
  "Return the text of TOKEN."
  (nth 1 token))

(defun swift-mode:token:start (token)
  "Return the start position of TOKEN."
  (nth 2 token))

(defun swift-mode:token:end (token)
  "Return the end position of TOKEN."
  (nth 3 token))

;; Token types is one of the follwing symbols:
;;
;; - prefix-operator (including try, try?, and try!)
;; - postfix-operator
;; - binary-operator (including as, as?, as!, is, =, ., and ->)
;; - attribute (e.g. @objc, @abc(def))
;; - identifier (including keywords, numbers, implicit parameters, and unknown tokens)
;; - [
;; - ]
;; - {
;; - }
;; - (
;; - )
;; - ,
;; - ;
;; - implicit-;
;; - < (as an angle bracket)
;; - > (as an angle bracket)
;; - supertype-: (colon for supertype declaration or type declaration of let or var)
;; - case-: (colon for case or default label)
;; - : (part of conditional operator, key-value separator, label-statement separator)
;; - anonymous-function-parameter-in ("in" after anonymous function parameter)
;; - outside-of-buffer
;;
;; Additionaly, `swift-mode:backward-token-or-list' may return a parenthesized
;; expression as a token with one of the following types:
;; - ()
;; - []
;; - {}
;; - <>

;;; Syntax table

(defconst swift-mode:syntax-table
  (let ((table (make-syntax-table)))
    ;; Whitespace characters
    ;; Word constituents

    ;; Symbol constituents
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?# "_" table)

    ;; Punctuation characters
    ;;
    ;; Operators
    ;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410
    ;; TODO Unicode operators
    ;;
    ;; / and * will be overridden below as comment delimiters
    (mapc (lambda (c) (modify-syntax-entry c "." table)) "/=-+!*%<>&|^~?.")
    ;; Separators
    (mapc (lambda (c) (modify-syntax-entry c "." table)) ",;")

    ;; Parenthesis characters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)

    ;; String quotes
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?` "\"" table)

    ;; Escape-syntax characters
    (modify-syntax-entry ?\\ "\\" table)

    ;; Character quotes
    ;; Paired delimiters
    ;; Expression prefixes

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\r "> b"    table)

    table))


;;; Lexers

(defun swift-mode:implicit-semi-p ()
  "Return t if the cursor is after the end of a statement."
  (let
      ((previous-token (save-excursion
                         (swift-mode:backquote-identifier-if-after-dot
                          (swift-mode:backward-token-simple))))
       (next-token (save-excursion
                     (swift-mode:backquote-identifier-if-after-dot
                      (swift-mode:forward-token-simple)))))
    ;; If the cursor is on the empty line, pretend an identifier is the line.
    (when (and
           (< (swift-mode:token:end previous-token) (line-beginning-position))
           (< (line-end-position) (swift-mode:token:start next-token)))
      (setq next-token (swift-mode:token 'identifier "" (point) (point))))
    (cond
     ((or
       ;; Supresses implicit semicolon around binary operators and separators.
       (memq (swift-mode:token:type previous-token)
             '(binary-operator \; \, :))
       (memq (swift-mode:token:type next-token)
             '(binary-operator \; \, :))

       ;; Supresses implicit semicolon after try, try?, and try!.
       (member (swift-mode:token:text previous-token)
               '("try" "try?" "try!"))

       ;; Suppress implicit semicolon after open brackets or before close
       ;; brackets.
       (memq (swift-mode:token:type previous-token) '({ \( \[))
       (memq (swift-mode:token:type next-token) '(} \) \]))

       ;; Suppress implicit semicolon around keywords that cannot start or end
       ;; statements.
       (member (swift-mode:token:text previous-token)
               '("inout" "throws" "rethrows" "in" "where"))
       (member (swift-mode:token:text next-token)
               '("inout" "throws" "rethrows" "in" "where")))
      nil)

     ;; Inserts semicolon before open curly bracket.
     ;;
     ;; Open curly bracket may continue the previous line, but we do not indent
     ;; there. For example, the code below is parsed as `(foo() { x in ... })'
     ;; by the Swift compiler, but we indent it like `foo(); { x in ... }'.
     ;;
     ;; foo()
     ;; { // does not indent here
     ;;   x in
     ;;   ...
     ;; }
     ((eq (swift-mode:token:type next-token) '\{) t)

     ;; Inserts implicit semicolon around #... directives.
     ;;
     ;; Note that we cannot split #if line; the following code is not allowed.
     ;;
     ;; #if
     ;;   true
     ;; #end if
     ((and
       (or
        (string-prefix-p "#" (swift-mode:token:text previous-token))
        (string-prefix-p "#" (swift-mode:token:text next-token)))
       (not (member (swift-mode:token:text previous-token)
                    '("#file" "#line" "column" "#function")))
       (not (member (swift-mode:token:text next-token)
                    '("#file" "#line" "column" "#function"))))
      t)

     ;; Supress implicit semicolon after attributes.
     ((eq (swift-mode:token:type previous-token) 'attribute)
      nil)

     ;; Suppress implicit semicolon after modifiers.
     ((member (swift-mode:token:text previous-token)
              '("indirect" "convenience" "dynamic" "final" "infix" "lazy"
                "mutating" "nonmutating" "optional" "override" "postfix"
                "prefix" "required" "static" "unowned" "weak" "internal"
                "private" "public" "open" "fileprivate"))
      nil)

     ;; internal(set) private(set) public(set) open(set) fileprivate(set)
     ;; unowned(safe) unowned(unsafe)
     ((and
       (eq (swift-mode:token:type previous-token) '\))
       (save-excursion
         (and
          (eq (swift-mode:token:type (swift-mode:backward-token-simple)) '\))
          (member (swift-mode:token:text (swift-mode:backward-token-simple))
                  '("set" "safe" "unsafe"))
          (eq (swift-mode:token:type (swift-mode:backward-token-simple)) '\()
          (member (swift-mode:token:text
                   (swift-mode:backquote-identifier-if-after-dot
                    (swift-mode:backward-token-simple)))
                  '("unowned" "internal" "private" "public" "open"
                    "fileprivate")))))
      nil)

     ;; Insert implicit semicolon before modifiers.
     ;;
     ;; Preceding mofidiers takes precedence over this.
     ((member (swift-mode:token:text next-token)
              '("indirect" "convenience" "dynamic" "final" "infix" "lazy"
                "mutating" "nonmutating" "optional" "override" "postfix"
                "prefix" "required" "static" "unowned" "weak" "internal"
                "private" "public" "open" "fileprivate"))
      t)

     ;; Inserts implicit semicolon around keywords that forms single keyword
     ;; statements.
     ((or
       (member (swift-mode:token:text previous-token)
               '("break" "continue" "fallthrough"))
       (member (swift-mode:token:text next-token)
               '("break" "continue" "fallthrough")))
      t)

     ;; Suppress implicit semicolon after keywords that cannot end statements.
     ((member (swift-mode:token:text previous-token)
              '("while" "for" "switch" "case" "default" "catch" "if" "guard"
                "let" "var" "throw" "import"))
      nil)

     ;; Inserts implicit semicolon before keywords that starts a new
     ;; statement.
     ((member (swift-mode:token:text next-token)
              '("for" "repeat" "switch"  "case" "default" "defer" "do" "if"
                "guard" "let" "var" "throw" "import" "return"))
      t)

     ;; Suppress implicit semicolon after return.
     ((equal (swift-mode:token:text previous-token) "return")
      nil)

     ;; Inserts implicit semicolon before `while' unless it is part of
     ;; `repeat...while'.
     ((equal (swift-mode:token:text next-token) "while")
      (save-excursion
        (not
         (and
          (eq (swift-mode:token:type previous-token) '\})
          (progn
            (backward-list)
            (equal (swift-mode:token:text
                    (swift-mode:backquote-identifier-if-after-dot
                     (swift-mode:backward-token-simple)))
                   "repeat"))))))

     ;; Inserts implicit around else
     ((or
       (equal (swift-mode:token:text previous-token) "else")
       (equal (swift-mode:token:text next-token) "else"))
      t)

     ;; Inserts implicit semicolon before keywords that behave like method
     ;; names.
     ((member (swift-mode:token:text next-token)
              '("get" "set" "willSet" "didSet" "subscript" "init" "deinit"))
      t)

     ;; Suppress implicit semicolon after declaration starters.
     ((member (swift-mode:token:text previous-token)
              '("class" "struct" "protocol" "enum" "extension" "func"
                "typealias""associatedtype" "precedencegroup" "operator"))
      nil)

     ;; Inserts implicit semicolon before declaration starters.
     ;; Modifiers take precedence over this.
     ;;
     ;; Notes that class-requirement is handled by the `:' rule above:
     ;;
     ;; protocol Foo: // not insert semicolon here
     ;;   class
     ;;
     ;; `protocol' is handled by the next rule
     ((member (swift-mode:token:text next-token)
              '("class" "struct" "enum" "extension" "func" "typealias"
                "associatedtype" "precedencegroup"))
      t)

     ;; Inserts implicit semicolon before protocol unless it is followed by <.
     ((equal "protocol" (swift-mode:token:text next-token))
      (not (equal (swift-mode:token:text
                   (save-excursion
                     (swift-mode:forward-token-simple)
                     (swift-mode:forward-token-simple)))
                  "<")))

    ;; Suppress implicit semicolon after keywords that behave like method
    ;; names.
    ;;
    ;; Note that binary operators take precedence over this:
    ;;
    ;; self . // not insert semicolon here
    ;;   init
    ;;
    ;; var x {
    ;;   set // not insert semicolon here
    ;;     (x) {
    ;;   }
    ;; }
    ;;
    ;; var x {
    ;;   set // inserts semicolon here
    ;;   {
    ;;   }
    ;; }
    ((member (swift-mode:token:text previous-token)
             '("set" "willSet" "didSet" "subscript" "init" "deinit"))
     nil)

    ;; Inserts implicit semicolon before open square bracket.
    ;;
    ;; Open square bracket for array indexing cannot appear at the start of a
    ;; line.
    ;; https://github.com/apple/swift/blob/8d4b1cc3c47c7624d57f188d5b227152ccb03163/lib/Parse/ParseExpr.cpp#L1525
    ;;
    ;; Note that the following pattern (i.e. after binary-operator) is handled
    ;; by above case.
    ;;
    ;; let x =
    ;;   [
    ;;     1
    ;;   ]
    ((eq (swift-mode:token:type next-token) '\[) t)

    ;; Inserts implicit semicolon before open parenthesis.
    ;;
    ;; Open parenthesis for function arguments cannot appear at the start of a
    ;; line.
    ;; https://github.com/apple/swift/blob/8d4b1cc3c47c7624d57f188d5b227152ccb03163/lib/Parse/ParseExpr.cpp#L1251
    ;;
    ;; Note that the following pattern (i.e. after binary-operator) is handled
    ;; by above case.
    ;;
    ;; let x =
    ;;   (
    ;;     1
    ;;   )
    ((eq (swift-mode:token:type next-token) '\() t)

    ;; Otherwise, inserts implicit semicolon.
    (t t))))

(defun swift-mode:supertype-colon-p ()
  "Return t if a colon at the cursor is the colon for supertype.

That is supertype declaration or type declaration of let or var."
  (save-excursion
    (let ((previous-token (swift-mode:backward-token-simple)))
      ;; class Foo<T>: Bar ← supertype colon
      ;; class Foo<T> : Bar ← supertype colon
      ;; class Foo<T where T: Bar<[(Int, String)]>> : Bar ← supertype colon
      ;; case Foo: ← not a supertype colon
      ;; case Foo where foo: ← not a supertype colon
      ;; case let Foo(x) where x is Foo<Int>: ← not a supertype colon
      ;; default: ← not a supertype colon
      ;; foo ? bar : baz ← not a supertype colon
      ;; [
      ;;   foo: ← not a supertype colon
      ;;     bar
      ;; ]
      ;; foo(bar, baz: baz) ← not a supertype colon
      ;; protocol Foo {
      ;;   associatedtype Bar<A>: Baz ← supertype colon
      ;; }
      (or
       ;; FIXME case let Foo(x) where x is Foo<Int>
       (eq (swift-mode:token:type previous-token) '>)
       ;; class Foo: ← supertype colon
       ;; extension Foo: ← supertype colon
       ;; let foo: ← not a supertype colon
       ;; var foo: ← not a supertype colon
       ;; protocol Foo {
       ;;   associatedtype Bar: Baz ← supertype colon
       ;; }
       (member (swift-mode:token:text
                (swift-mode:backquote-identifier-if-after-dot
                 (swift-mode:backward-token-simple)))
               '("class" "extension" "enum" "struct" "protocol" "typealias"
                 "associatedtype"))))))

(defvar swift-mode:in-recursive-call-of-case-colon-p nil
  "Non-nil if `case-colon-p' is being evaluated.")

(defun swift-mode:case-colon-p ()
  "Return non-nil if the colon at the cursor follows case or default label.

Return nil otherwise."
  (if swift-mode:in-recursive-call-of-case-colon-p
      nil
    (save-excursion
      (setq swift-mode:in-recursive-call-of-case-colon-p t)

      (unwind-protect
          (member
           ;; FIXME:
           ;; This function can be confused by conditional operator.
           ;;
           ;; switch foo {
           ;; case let x where x is Foo ?
           ;;                    a : // This function should return nil but it
           ;;                       // actually retuns t.
           ;;                    b: // This function should return t but it
           ;;                       // actually return nil.
           ;;   let y = a ? b : c // This function returns nil correctly for this.
           ;; }

           ;; FIXME: mutual dependency
           (swift-mode:token:text
            (swift-mode:backward-sexps-until
             '(implicit-\; \; { \( \[ "case" "default" ":")))
           '("case" "default"))
        (setq swift-mode:in-recursive-call-of-case-colon-p nil)))))

(defun swift-mode:anonyous-parameter-in-p ()
  "Return t if a 'in' token at the cursor is for anonymous function parameters."
  (save-excursion
    (eq
     ;; FIXME: mutual dependency
     (swift-mode:token:type (swift-mode:backward-sexps-until
                             '(\; { \( \[ "for")))
     '{)))

(defun swift-mode:fix-operator-type (token)
  "Return new operator token with proper token type.

Other properties are the same as the TOKEN."
  ;; Operator type (i.e. prefix, postfix, infix) is decided from spaces or
  ;; comments around the operator.
  ;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410
  ;; https://github.com/apple/swift-evolution/blob/master/proposals/0037-clarify-comments-and-operators.md
  (let*
      ((text (swift-mode:token:text token))
       (start (swift-mode:token:start token))
       (end (swift-mode:token:end token))
       (has-preceding-space (or
                             (= start (point-min))
                             (memq (char-syntax (char-before start)) '(?  ?>))
                             (nth 4 (save-excursion
                                      (syntax-ppss (1- start))))))
       (has-following-space (or
                             (= end (point-max))
                             (memq (char-syntax (char-after end)) '(?  ?<))
                             (save-excursion (goto-char end)
                                             (looking-at "/\\*\\|//"))
                             (= (char-after end) ?\C-j)))
       (has-following-dot (eq (char-after end) ?.))
       (is-declaration (save-excursion
                         ;; i.e.
                         ;; func +++(x1: X, x2: X)
                         ;; or operator declarations.
                         (goto-char start)
                         (member
                          (swift-mode:token:text
                           (swift-mode:backquote-identifier-if-after-dot
                            (swift-mode:backward-token-simple)))
                          '("func" "operator"))))
       (type
        (cond
         (is-declaration 'identifier)
         ((member text '("try" "try?" "try!")) 'prefix-operator)
         ((equal text ".") 'binary-operator)
         ((and has-preceding-space has-following-space) 'binary-operator)
         (has-preceding-space 'prefix-operator)
         ((or has-following-space has-following-dot) 'postfix-operator)
         (t 'binary-operator))))
    (swift-mode:token type text start end)))

(defun swift-mode:backquote-identifier-if-after-dot (token)
  "Backquote identifier TOKEN, including keywords, if it is after a dot.

See SE-0071:
https://github.com/apple/swift-evolution/blob/master/proposals/0071-member-keywords.md"
  (if (and (string-match "^[a-z]" (swift-mode:token:text token))
           (save-excursion
             (goto-char (swift-mode:token:start token))
             (equal (swift-mode:token:text (swift-mode:backward-token-simple))
                    ".")))
      (swift-mode:token
       'identifier
       (concat "`" (swift-mode:token:text token) "`")
       (swift-mode:token:start token)
       (swift-mode:token:end token))
    token))

(defun swift-mode:forward-token ()
  "Move point forward to the next position of the end of a token.

Return the token object.  If no more tokens available, return a token with
type `out-of-buffer'"

  (let ((pos (point)))
    (when (nth 4 (syntax-ppss))
      (goto-char (nth 8 (syntax-ppss))))
    (forward-comment (point-max))
    (cond
     ;; Outside of buffer
     ((eobp)
      (swift-mode:token 'outside-of-buffer "" (point) (point)))

     ;; Implicit semicolon
     ((and (< pos
              (save-excursion
                (swift-mode:goto-non-comment-bol)
                (point)))
           (save-excursion (goto-char pos) (swift-mode:implicit-semi-p)))

      (swift-mode:token 'implicit-\;
                        (buffer-substring-no-properties pos (point))
                        pos
                        (point)))

     ;; Colon
     ((eq (char-after) ?:)
      (swift-mode:token (cond
                         ((swift-mode:supertype-colon-p) 'supertype-:)
                         ((swift-mode:case-colon-p) 'case-:)
                         (t ':))
                        ":"
                        (progn (forward-char) (1- (point)))
                        (point)))

     (t
      (let ((token (swift-mode:forward-token-simple)))
        (setq token (swift-mode:backquote-identifier-if-after-dot token))

        (when (and (equal (swift-mode:token:text token) "in")
                   (save-excursion
                     (goto-char (swift-mode:token:start token))
                     (swift-mode:anonyous-parameter-in-p)))
          (setq token
                (swift-mode:token
                 'anonymous-function-parameter-in
                 "in"
                 (swift-mode:token:start token)
                 (swift-mode:token:end token))))
        token)))))

(defun swift-mode:forward-token-simple ()
  "Like `swift-mode:forward-token' without recursion.

This function does not return `implicit-;' or `type-:'."
  (forward-comment (point-max))
  (cond
   ;; Outside of buffer
   ((eobp)
    (swift-mode:token 'outside-of-buffer "" (point) (point)))

   ;; Separators and parentheses
   ((memq (char-after) '(?, ?\; ?\{ ?\} ?\[ ?\] ?\( ?\) ?:))
    (forward-char)
    (swift-mode:token (intern (string (char-before)))
                      (string (char-before))
                      (1- (point))
                      (point)))

   ;; Open angle bracket for type parameters
   ;;
   ;; We use a heuristic: spaces are inserted around inequality sign, but not
   ;; for angle bracket, and a type paramer starts with an upper case
   ;; character, a square bracket, a parenthesis, or keyword 'protocol'.
   ((and (eq (char-after) ?<)
         (looking-at "<\\([[:upper:]\\[[(]\\|protocol\\)"))
    (forward-char)
    (swift-mode:token '< "<" (1- (point)) (point)))

   ;; Close angle bracket for type parameters
   ;;
   ;; Close angle bracket follows identifier, a square bracket, a parenthesis,
   ;; or another another bracket (e.g. Foo<Bar<[(Int, String)]>>)
   ((and (eq (char-after) ?>)
         (save-excursion
           ;; You know that regular language can be reversed. Thus you may
           ;; think that `looking-back' reverse the given regexp and scan
           ;; chars backwards. Nevertheless, `looking' function does not
           ;; do that. It just repeats `looking-at' with decrementing start
           ;; position until it succeeds. The document says that it is not
           ;; recommended to use. So using `skip-chars-backward',
           ;; `skip-syntax-backward', and `looking-at' here.
           (skip-chars-backward "])>")
           (skip-syntax-backward "w_")
           (looking-at "[[:upper:]_]")))
    (forward-char)
    (swift-mode:token '> ">" (1- (point)) (point)))

   ;; Operator (other than as, try, or is)
   ;;
   ;; Operators starts with a dot can contains dots. Other operators cannot
   ;; contain dots.
   ;;
   ;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/swift/grammar/dot-operator-head
   ((looking-at "[-/=+!*%<>&|^~?]+\\|[.][-./=+!*%<>&|^~?]*")
    (let*
        ((text (match-string-no-properties 0))
         (start (match-beginning 0))
         (end (match-end 0)))
      (when (string-match ".*/\\*\\|.*//" text)
        ;; e.g. +++/* */ or +++//
        (setq end
              (- end
                 (- (length text) (- (match-end 0) 2))))
        (setq text (substring text 0 (- (match-end 0) 2))))
      (goto-char end)
      (swift-mode:fix-operator-type
       (swift-mode:token nil text start end))))

   ;; String and backquoted identifer
   ((memq (char-after) '(?\" ?`))
    (let ((pos-after-comment (point)))
      (goto-char (scan-sexps (point) 1))
      (swift-mode:token
       'identifier
       (buffer-substring-no-properties pos-after-comment (point))
       pos-after-comment
       (point))))

   ;; Attribute
   ((eq (char-after) ?@)
    (let ((pos-after-comment (point)))
      (forward-symbol 1)
      (let ((pos (point)))
        (forward-comment (point-max))
        (if (eq (char-after) ?\()
            (condition-case nil
                (progn
                  (forward-list 1))
              (scan-error (goto-char pos)))
          (goto-char pos)))
      (swift-mode:token
       'attribute
       (buffer-substring-no-properties pos-after-comment (point))
       pos-after-comment
       (point))))

   ;; Other tokens including identifers, implicit parameters, keywords, and
   ;; numbers
   (t
    (let*
        ((pos-after-comment (point))
         (text
          (cond
           ;; Identifers, implicit parameters, keywords, numbers
           ;;
           ;; Note: syntax class _ includes #, @, and $.
           ((memq (char-syntax (char-after)) '(?w ?_))
            (progn (forward-symbol 1)
                   (buffer-substring-no-properties pos-after-comment
                                                   (point))))
           ;; Unknown character type. Treats as a single-letter token.
           (t (forward-char) (string (char-before))))))
      (cond
       ((member text '("as" "try"))
        ;; as?, as!, try?, or try!
        (when (member (char-after) '(?? ?!))
          (forward-char)
          (setq text (concat text (list (char-before)))))
        (swift-mode:token (if (member text '("as" "as?" "as!"))
                              'binary-operator
                            'prefix-operator)
                          text
                          (- (point) (length text))
                          (point)))
       ((equal text "is")
        (swift-mode:token 'binary-operator
                          text
                          (- (point) (length text))
                          (point)))
       (t
        (swift-mode:token 'identifer
                          text
                          (- (point) (length text))
                          (point))))))))

(defun swift-mode:backward-token ()
  "Move point backward to the previous position of the end of a token.

Return the token object.  If no more tokens available, return a token with
type `out-of-buffer'."

  (let ((pos (point)))
    (when (nth 4 (syntax-ppss))
      (goto-char (nth 8 (syntax-ppss))))
    (forward-comment (- (point)))
    (cond
     ;; Outside of buffer
     ((bobp)
      (swift-mode:token 'outside-of-buffer "" (point) (point)))

     ;; Implicit semicolon
     ((and (< (save-excursion
                (swift-mode:goto-non-comment-eol)
                (point))
              pos)
           (save-excursion (goto-char pos) (swift-mode:implicit-semi-p)))
      (swift-mode:token 'implicit-\;
                        (buffer-substring-no-properties (point) pos)
                        (point)
                        pos))

     ;; Colon
     ((eq (char-before) ?:)
      (backward-char)
      (swift-mode:token (cond
                         ((swift-mode:supertype-colon-p) 'supertype-:)
                         ((swift-mode:case-colon-p) 'case-:)
                         (t ':))
                        ":"
                        (point)
                        (1+ (point))))

     (t
      (let ((token (swift-mode:backward-token-simple)))
        (setq token (swift-mode:backquote-identifier-if-after-dot token))

        (when (and (equal (swift-mode:token:text token) "in")
                   (save-excursion
                     (goto-char (swift-mode:token:start token))
                     (swift-mode:anonyous-parameter-in-p)))
          (setq token
                (swift-mode:token
                 'anonymous-function-parameter-in
                 "in"
                 (swift-mode:token:start token)
                 (swift-mode:token:end token))))
        token)))))

(defun swift-mode:backward-token-simple ()
  "Like `swift-mode:backward-token' without recursion.

This function does not return `implicit-;' or `type-:'."
  (forward-comment (- (point)))
  (cond
   ;; Outside of buffer
   ((bobp)
    (swift-mode:token 'outside-of-buffer "" (point) (point)))

   ;; Attribute or close-parenthesis
   ((eq (char-before) ?\))
     (let ((pos-before-comment (point)))
       (condition-case nil
           (progn
             (backward-list)
             (forward-comment (- (point)))
             (forward-symbol -1)
             (unless (eq (char-after) ?@)
               (goto-char (1- pos-before-comment))))
         (scan-error (goto-char (1- pos-before-comment))))
       (swift-mode:token
        (if (eq (char-after) ?@) 'attribute '\))
        (buffer-substring-no-properties (point) pos-before-comment)
        (point)
        pos-before-comment)))

   ;; Separators and parentheses
   ((memq (char-before) '(?, ?\; ?\{ ?\} ?\[ ?\] ?\( ?\) ?:))
    (backward-char)
    (swift-mode:token (intern (string (char-after)))
                      (string (char-after))
                      (point)
                      (1+ (point))))

   ;; >! or >?
   ((and (memq (char-before) '(?! ??))
         (eq (char-before (1- (point))) ?>)
         (save-excursion
           (backward-char)
           (eq (swift-mode:token:type (swift-mode:backward-token-simple))
               '>)))
    (backward-char)
    (swift-mode:token (intern (string (char-after)))
                      (string (char-after))
                      (point)
                      (1+ (point))))

   ;; Open angle bracket for type parameters
   ;;
   ;; We use a heuristic: spaces are inserted around inequality sign, but not
   ;; for angle bracket, and a type paramer starts with an upper case
   ;; character, a square bracket, a parenthesis, or keyword `protocol'.
   ((and (eq (char-before) ?<)
         (looking-at "\\([[:upper:]\\[[(]\\|protocol\\)"))
    (backward-char)
    (swift-mode:token '< "<" (point) (1+ (point))))

   ;; Close angle bracket for type parameters
   ;;
   ;; Close angle bracket follows identifier, a square bracket, a parenthesis,
   ;; or another another bracket (e.g. Foo<Bar<[(Int, String)]>>)
   ((and (eq (char-before) ?>)
         (save-excursion
           (skip-chars-backward "])>")
           (skip-syntax-backward "w_")
           (looking-at "[[:upper:]_]")))
    (backward-char)
    (swift-mode:token '> ">" (point) (1+ (point))))

   ;; Operator (other than as, try, or is)
   ;;
   ;; Operators which starts with a dot can contain other dots. Other
   ;; operators cannot contain dots.
   ;;
   ;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/swift/grammar/dot-operator-head
   ((memq (char-before) '(?. ?- ?/ ?= ?+ ?! ?* ?% ?< ?> ?& ?| ?^ ?~ ??))
    (let ((point-before-comments (point)))
      (skip-chars-backward "-./=+!*%<>&|^~?")
      (cond
       ((save-excursion
          (forward-symbol -1)
          (and (looking-at "\\(as\\|try\\)[?!]")
               (= point-before-comments (match-end 0))))
        ;; as?, as!, try?, try!
        t)
       ((looking-at "[.][-./=+!*%<>&|^~?]*")
        ;; e.g. 1 .++++.++++...+. 1
        t)
       ((and (looking-at "[-/=+!*%<>&|^~?]+")
             (<= point-before-comments (match-end 0)))
        ;; e.g. 1 +++++++++ 1
        t)
       (t
        ;; e.g. 1+++++...++++1, that is (1+++++) ...++++ 1
        (skip-chars-forward "-/=+!*%<>&|^~?")
        (looking-at "[.][-./=+!*%<>&|^~?]*")))
      (let*
          ((start (match-beginning 0))
           (end (min point-before-comments (match-end 0)))
           (text (substring (match-string-no-properties 0) 0 (- end start))))
        (goto-char start)
        (swift-mode:fix-operator-type
         (swift-mode:token nil text start end)))))

   ;; String and backquoted identifer
   ((memq (char-before) '(?\" ?`))
    (let ((pos-before-comment (point)))
      (goto-char (scan-sexps (point) -1))
      (swift-mode:token
       'identifier
       (buffer-substring-no-properties (point) pos-before-comment)
       (point)
       pos-before-comment)))

   ;; Other tokens including identifers, implicit parameters, keywords, and
   ;; numbers
   (t
    (let*
        ((pos-before-comment (point))
         (text
          (cond
           ;; Identifers, implicit parameters, keywords, numbers
           ;;
           ;; Note: syntax class _ includes #, @, and $.
           ((memq (char-syntax (char-before)) '(?w ?_))
            (progn (forward-symbol -1)
                   (buffer-substring-no-properties (point)
                                                   pos-before-comment)))
           ;; Unknown character type. Treats as a single-letter token.
           (t (backward-char) (string (char-after))))))
      (cond
       ((member text '("is" "as"))
        (swift-mode:token 'binary-operator
                          text
                          (point)
                          (+ (point) (length text))))
       ((equal text "try")
        (swift-mode:token 'prefix-operator
                          text
                          (point)
                          (+ (point) (length text))))
       ((string-prefix-p "@" text)
        (swift-mode:token 'attribute
                          text
                          (point)
                          (+ (point) (length text))))
       (t
        (swift-mode:token 'identifier
                          text
                          (point)
                          (+ (point) (length text)))))))))

(defun swift-mode:goto-non-comment-bol ()
  "Back to the beginning of line that is not inside a comment."
  (beginning-of-line)
  (while (nth 4 (syntax-ppss))
    ;; The cursor is in a comment. Backs to the beginning of the comment.
    (goto-char (nth 8 (syntax-ppss)))
    (beginning-of-line)))

(defun swift-mode:goto-non-comment-eol ()
  "Proceed to the end of line that is not inside a comment.

If this line ends with a single-line comment, goto just before the comment."
  (end-of-line)
  (while (nth 4 (syntax-ppss))
    ;; The cursor is in a comment.
    (if (eq (nth 4 (syntax-ppss)) t)
        ;; This ia a single-line comment
        ;; Back to the beginning of the comment.
        (goto-char (nth 8 (syntax-ppss)))
      ;; This is a multiline comment
      ;; Proceed to the end of the comment.
      (goto-char (nth 8 (syntax-ppss)))
      (forward-comment 1)
      (end-of-line))))

(provide 'swift-mode-lexer)

;;; swift-mode-lexer.el ends here
