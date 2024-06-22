;;; swift-mode-lexer.el --- Major-mode for Apple's Swift programming language, lexer. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021 taku0, Chris Barrett, Bozhidar Batsov,
;;                         Arthur Evstifeev

;; Author: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>

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

;; Terminology:
;; (See also docs/string.png)
;;
;;   Interpolated string:
;;     A string containing expressions to be evaluated and inserted into the
;;     string at run time.
;;     Example: "1 + 1 = \(1 + 1)" is evaluated to "1 + 1 = 2" at run time.
;;     https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html
;;     https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html
;;
;;   Interpolated expression:
;;     An expression between \( and ) inside a string.
;;     Suppose a string "aaa\( foo() )bbb\( bar() )ccc",
;;     `foo()' and `bar()' are interpolated expression.
;;     https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html
;;     Why not interpolating expression?
;;
;;   String chunk:
;;     A part of single-line/multiline string delimited with quotation marks
;;     or interpolated expressions.
;;     Suppose a string "aaa\( foo() )bbb\( bar() )ccc",
;;     "aaa\(, )bbb\(, and )ccc" are string chunks.
;;
;;     This is not a official term; used only in swift-mode.

(declare-function swift-mode:backward-sexps-until "swift-mode-indent.el"
                  (token-types
                   &optional
                   stop-at-eol-token-types
                   stop-at-bol-token-types))

(declare-function swift-mode:try-backward-generic-parameters
                  "swift-mode-indent.el"
                  ())


(defun swift-mode:token (type text start end)
  "Construct and return a token.

TYPE is the type of the token such as `binary-operator' or {.
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

;; Token types is one of the following symbols:
;;
;; - prefix-operator (including try, try?, try!, await, consume, copy, discard,
;;   and each)
;; - postfix-operator
;; - binary-operator (including as, as?, as!, is, =, ., and ->)
;; - attribute (e.g. @objc, @abc(def))
;; - identifier (including keywords, numbers, implicit parameters, and unknown
;;   tokens)
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
;; - supertype-: (colon for supertype declaration or type declaration of let or
;;   var)
;; - case-: (colon for case or default label)
;; - : (part of conditional operator, key-value separator, label-statement
;;   separator)
;; - anonymous-function-parameter-in ("in" after anonymous function parameter)
;; - string-chunk-after-interpolated-expression (part of a string starting with
;;   ")")
;; - string-chunk-before-interpolated-expression (part of a string ending with
;;   "\\(")
;; - outside-of-buffer
;;
;; Additionally, `swift-mode:backward-token-or-list' may return a parenthesized
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

;;; Text properties

;; (See also docs/string_properties.png)
;;
;; Some properties are put by `syntax-propertize-function', that is
;; `swift-mode:syntax-propertize'.
;;
;; The beginning of and end of strings are marked with text property
;; '(syntax-table (15)), which indicates generic string delimiters. Both
;; single-line strings and multiline strings are marked with it. The
;; parentheses surrounding interpolated expressions are also marked with
;; '(syntax-table (15)). The property helps font-lock and the tokenizer to
;; recognize strings.
;;
;; The entire string, including interpolated expressions, are marked with text
;; property '(syntax-multiline t). The property is used by
;; `swift-mode:syntax-propertize-extend-region' to avoid scanning from the
;; middle of strings.
;;
;; The parentheses surrounding interpolated expressions have text property
;; '(swift-mode:matching-parenthesis POS), where POS is the position of the
;; matching parenthesis. Strictly speaking, the POS on the closing parenthesis
;; refers to the backslash before the opening parenthesis. The property speeds
;; up the indentation logic.

(defun swift-mode:syntax-propertize-extend-region (start end)
  "Return region to be propertized.
The returned region contains the region (START . END).
If the region is not modified, return nil.
Intended for `syntax-propertize-extend-region-functions'."
  (syntax-propertize-multiline start end))

(defun swift-mode:syntax-propertize (start end)
  "Update text properties for strings.
Mark the beginning of and the end of single-line/multiline strings and regexes
between the position START and END as general string delimiters.
Intended for `syntax-propertize-function'."
  (remove-text-properties start end
                          '(syntax-table
                            nil
                            syntax-multiline
                            nil
                            swift-mode:matching-parenthesis
                            nil
                            swift-mode:comment
                            nil))
  (let* ((chunk (swift-mode:chunk-after (syntax-ppss start)))
         comment-start)
    (cond
     ((swift-mode:chunk:multiline-string-p chunk)
      (swift-mode:syntax-propertize:end-of-string
       end "\"\"\"" (swift-mode:chunk:pound-count chunk)))

     ((swift-mode:chunk:single-line-string-p chunk)
      (swift-mode:syntax-propertize:end-of-string
       end "\"" (swift-mode:chunk:pound-count chunk)))

     ((swift-mode:chunk:regex-p chunk)
      (swift-mode:syntax-propertize:end-of-regex
       (swift-mode:chunk:start chunk)))

     ((swift-mode:chunk:comment-p chunk)
      (goto-char (swift-mode:chunk:start chunk))
      (setq comment-start (point))
      (forward-comment 1)
      (put-text-property comment-start (point) 'swift-mode:comment t))))
  (swift-mode:syntax-propertize:scan end 0))

(defun swift-mode:syntax-propertize:scan (end nesting-level)
  "Update text properties for strings.
Mark the beginning of and the end of single-line/multiline strings and regexes
between the current position and END as general string delimiters.
Assuming the cursor is not on strings, regexes, nor comments.
If NESTING-LEVEL is non-zero, nesting of parentheses are tracked and the scan
stops where the level becomes zero."
  (let ((found-matching-parenthesis nil)
        (pattern (mapconcat #'regexp-quote
                            '("\"\"\"" "\"" "/" "(" ")")
                            "\\|")))
    (while (and (not found-matching-parenthesis)
                (< (point) end)
                (search-forward-regexp pattern end t))
      (cond
       ((member (match-string-no-properties 0) '("\"\"\"" "\""))
        (let ((start (match-beginning 0))
              (quotation (match-string-no-properties 0))
              pound-count)
          (save-excursion
            (goto-char start)
            (skip-chars-backward "#")
            (setq pound-count (- start (point)))
            (setq start (point)))
          (put-text-property start (1+ start)
                             'syntax-table
                             (string-to-syntax "|"))
          (swift-mode:syntax-propertize:end-of-string
           end quotation pound-count)
          (swift-mode:put-syntax-multiline-property start (point))))

       ((equal "/" (match-string-no-properties 0))
        (let ((start (match-beginning 0))
              regex-start)
          (save-excursion
            (goto-char start)
            (skip-chars-backward "#")
            (setq regex-start (point)))
          (cond
           ;; Regexes
           ((swift-mode:syntax-propertize:end-of-regex regex-start)
            (put-text-property regex-start (1+ regex-start)
                               'syntax-table
                               (string-to-syntax "|"))
            (swift-mode:put-syntax-multiline-property regex-start (point)))

           ;; Comments
           ((memq (char-after) '(?/ ?*))
            (goto-char start)
            (forward-comment 1)
            (put-text-property start (point) 'swift-mode:comment t))

           ;; Operators
           (t nil))))

       ((and (equal "(" (match-string-no-properties 0))
             (/= nesting-level 0))
        (setq nesting-level (1+ nesting-level)))

       ((and (equal ")" (match-string-no-properties 0))
             (/= nesting-level 0))
        (setq nesting-level (1- nesting-level))
        (when (= nesting-level 0)
          (setq found-matching-parenthesis t)))))
    (unless found-matching-parenthesis
      (goto-char end))
    found-matching-parenthesis))

(defun swift-mode:put-syntax-multiline-property (start end)
  "Put `syntax-multiline` text propery from START to END.

Also call `font-lock-flush' with START and END."
  (put-text-property start end 'syntax-multiline t)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush start end)
    (if (eq font-lock-fontify-buffer-function #'jit-lock-refontify)
        (jit-lock-refontify start end)
      (font-lock-after-change-function start end (- end start)))))

(defun swift-mode:syntax-propertize:end-of-string (end quotation pound-count)
  "Move point to the end of single-line/multiline string.

Assuming the cursor is on a string.
If the string go beyond END, stop there.
The string should be terminated with QUOTATION, followed by POUND-COUNT of
pound signs."
  (if (and
       (< (point) end)
       (search-forward-regexp (concat (regexp-quote quotation) "\\|(") end t))
      (cond
       ((and (equal quotation (match-string-no-properties 0))
             (not (swift-mode:escaped-p (match-beginning 0) pound-count))
             (progn
               (skip-chars-forward "#" (min end (+ (point) pound-count)))
               (= (- (point) (match-end 0)) pound-count)))
        (put-text-property (1- (point)) (point)
                           'syntax-table
                           (string-to-syntax "|")))

       ((and (equal "(" (match-string-no-properties 0))
             (swift-mode:escaped-p (match-beginning 0) pound-count))
        ;; Found an interpolated expression. Skips the expression.
        ;; We cannot use `scan-sexps' because multiline strings are not yet
        ;; propertized.
        (let ((pos-after-open-paren (point))
              (start
               (save-excursion
                 (backward-char) ;; (
                 (skip-chars-backward "#")
                 (backward-char) ;; \
                 (point))))
          ;; Declares the backslash is not a escape-syntax characters.
          (put-text-property start (1+ start)
                             'syntax-table
                             (string-to-syntax "w"))
          ;; Declares the open parentheses is a generic string delimiter.
          (put-text-property (1- pos-after-open-paren) pos-after-open-paren
                             'syntax-table
                             (string-to-syntax "|"))
          (when (swift-mode:syntax-propertize:scan end 1)
            ;; Found the matching parenthesis. Going further.
            ;; Declares the close parentheses is a generic string delimiter.
            (put-text-property (1- (point)) (point)
                               'syntax-table
                               (string-to-syntax "|"))
            ;; Records the positions.
            (put-text-property (1- (point)) (point)
                               'swift-mode:matching-parenthesis
                               start)
            (put-text-property start pos-after-open-paren
                               'swift-mode:matching-parenthesis
                               (1- (point)))
            (swift-mode:syntax-propertize:end-of-string
             end quotation pound-count))))

       (t
        (swift-mode:syntax-propertize:end-of-string end quotation pound-count)))
    (goto-char end)))


(defun swift-mode:escaped-p (position pound-count)
  "Return t if the POSITION in a string is escaped.

A position is escaped if it is proceeded by POUND-COUNT or more of pound signs
and odd number of backslashes.
Return nil otherwise."
  (let ((p position)
        (backslash-count 0))
    (while (eq (char-before p) ?#)
      (setq p (1- p)))
    (and
     ;; While it is a syntax error to have extra pound signs, we allow them
     ;; here to prevent corruption.
     (<= pound-count (- position p))
     (progn
       (while (eq (char-before p) ?\\)
         (setq backslash-count (1+ backslash-count))
         (setq p (1- p)))
       (= (mod backslash-count 2) 1)))))

(defun swift-mode:syntax-propertize:end-of-regex (start)
  "Move point to the end of regex if any.

START is the position of the open delimiter, including pounds if any.

If START is not a start of a regex, keep the point and return nil.  Otherwise,
return non-nil.

This function doesn't take end parameter since if the closing delimiter is
missing, this function must return nil."
  (let* ((pound-count (save-excursion
                        (goto-char start)
                        (skip-chars-forward "#")
                        (- (point) start)))
         end-of-regex)
    (setq end-of-regex
          (if (zerop pound-count)
              (swift-mode:syntax-propertize:end-of-basic-regex start)
            (swift-mode:syntax-propertize:end-of-extended-regex
             start
             pound-count)))
    (when end-of-regex
      (put-text-property (1- end-of-regex) end-of-regex
                         'syntax-table
                         (string-to-syntax "|")))
    end-of-regex))

(defun swift-mode:syntax-propertize:end-of-basic-regex (start)
  "Move point to the end of regex if any.

START is the position of the open delimiter.

If START is not a start of a regex, keep the point and return nil.  Otherwise,
return non-nil."
  (let* ((pos (point))
         (start-of-contents (1+ start))
         after-last-dot
         (square-brackets-count 0)
         (parentheses-count 0)
         (limit (line-end-position))
         (end-of-regex nil))
    (if (or
         ;; Cannot starts with spaces, tabs, slashes, or asterisks.
         (memq (char-after start-of-contents) '(?\s ?\t ?/ ?*))
         ;; Cannot be a comment closer: /**/+++/.
         (get-text-property start 'swift-mode:comment)
         ;; Cannot be preceded with infix operators while it can be preceded
         ;; with prefix operators.
         (save-excursion
           (goto-char start)
           ;; TODO Unicode operators
           (skip-chars-backward "-/=+!*%<>&|^~?")
           (when (eq (char-before) ?.)
             (setq after-last-dot (point))
             (skip-chars-backward "-/=+!*%<>&|^~?.")
             (unless (eq (char-after) ?.)
               (goto-char (1- after-last-dot))))
           (and
            ;; preceded with an operator
            (/= start (point))
            ;; it is not a prefix operator
            (not (memq (char-before)
                       '(nil ?\s ?\t ?\[ ?\( ?{ ?, ?\; ?:)))
            ;; it does't contain comments: a/**/+/**//b /
            (not (text-property-any (point) start 'swift-mode:comment t)))))
        nil
      (goto-char start-of-contents)
      (while (and (null end-of-regex)
                  (search-forward-regexp "[][()\\/]" limit t))
        (cond
         ((eq (char-before) ?\\)
          (forward-char))
         ((eq (char-before) ?\[)
          (setq square-brackets-count (1+ square-brackets-count)))
         ((eq (char-before) ?\])
          (when (< 0 square-brackets-count)
            (setq square-brackets-count (1- square-brackets-count))))
         ((eq (char-before) ?\()
          (when (zerop square-brackets-count)
            (setq parentheses-count (1+ parentheses-count))))
         ((eq (char-before) ?\))
          (cond
           ((< 0 square-brackets-count)
            nil)
           ((zerop parentheses-count)
            ;; Found an unmatching close parenthesis.  This is not a regex
            ;; literal.
            (goto-char limit))
           (t
            (setq parentheses-count (1- parentheses-count)))))
         ((eq (char-before) ?/)
          (if (memq (char-after) '(?/ ?*))
              (goto-char limit)
            (setq end-of-regex (point)))))))
    (when (and end-of-regex
               (memq (char-before (1- end-of-regex)) '(?\s ?\t))
               (not (swift-mode:escaped-p (- end-of-regex 2) 0)))
      ;; Cannot ends with spaces or tabs unless escaped.
      (setq end-of-regex nil))
    (unless end-of-regex
      (goto-char pos))
    end-of-regex))

(defun swift-mode:syntax-propertize:end-of-extended-regex (start pound-count)
  "Move point to the end of extended regex if any.

START is the position of the open delimiter, including pounds of POUND-COUNT.

If START is not a start of a regex, keep the point and return nil.  Otherwise,
return non-nil."
  (let* ((pos (point))
         (start-of-contents (1+ (+ start pound-count)))
         (starts-with-line-break (save-excursion
                                   (goto-char start-of-contents)
                                   (skip-chars-forward "\s\t")
                                   (eolp)))
         (end-of-regex nil))
    (goto-char start-of-contents)
    (if starts-with-line-break
        (while (and (null end-of-regex)
                    (zerop (forward-line)))
          (skip-chars-forward "\s\t")
          (when (and (eq (char-after) ?/)
                     (progn
                       (forward-char)
                       (eq (skip-chars-forward "#" (+ (point) pound-count))
                           pound-count)))
            (setq end-of-regex (point))))
      (while (and (null end-of-regex)
                  (search-forward-regexp "/#" (line-end-position) t))
        (backward-char)
        (when (and (eq (skip-chars-forward "#" (+ (point) pound-count))
                       pound-count)
                   ;; Inside regex literal, backslashes without pounds are
                   ;; still special.
                   (not (swift-mode:escaped-p (match-beginning 0) 0)))
          (setq end-of-regex (point)))))
    (unless end-of-regex
      (swift-mode:put-syntax-multiline-property start (point))
      (goto-char pos))
    end-of-regex))

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
    ;; If the cursor is on the empty line, pretend an identifier is on the line.
    (when (and
           (< (swift-mode:token:end previous-token) (line-beginning-position))
           (< (line-end-position) (swift-mode:token:start next-token)))
      (setq next-token (swift-mode:token 'identifier "" (point) (point))))
    (cond
     ((or
       ;; Suppresses implicit semicolon around binary operators and separators.
       (memq (swift-mode:token:type previous-token)
             '(binary-operator \; \, :))
       (memq (swift-mode:token:type next-token)
             '(binary-operator \; \, :))

       ;; Suppresses implicit semicolon after try, try?, try!, and await.
       (member (swift-mode:token:text previous-token)
               '("try" "try?" "try!" "await"))

       ;; Suppress implicit semicolon after open brackets or before close
       ;; brackets.
       (memq (swift-mode:token:type previous-token) '({ \( \[))
       (memq (swift-mode:token:type next-token) '(} \) \]))

       ;; Supress implicit semicolon before/after open angle bracket.
       (and (equal (swift-mode:token:text previous-token) "<")
            (save-excursion
              (goto-char (swift-mode:token:start previous-token))
              (swift-mode:generic-parameter-clause-start-p)))
       (and (equal (swift-mode:token:text next-token) "<")
            (save-excursion
              (goto-char (swift-mode:token:start next-token))
              (swift-mode:generic-parameter-clause-start-p)))

       ;; Suppress implicit semicolon after/before string chunks inside
       ;; interpolated expressions.
       (eq (swift-mode:token:type previous-token)
           'string-chunk-before-interpolated-expression)
       (eq (swift-mode:token:type next-token)
           'string-chunk-after-interpolated-expression)

       ;; Suppress implicit semicolon around keywords that cannot start or end
       ;; statements.
       (member (swift-mode:token:text previous-token)
               '("any" "some" "inout" "borrowing" "consuming" "in" "where"
                 "isolated" "each"))
       (member (swift-mode:token:text next-token)
               '("any" "some" "inout" "borrowing" "consuming" "throws"
                 "rethrows" "in" "where" "isolated" "each")))
      nil)

     ;; Before async
     ;;
     ;; Examples:
     ;;
     ;; func foo() async throws -> Void
     ;; foo { () async throws -> void in }
     ;; let f: () async throws -> Void = g
     ;; get async throws {}
     ;; async let x = foo()
     ;;
     ;; Suppresses implicit semicolon if and only if before let.
     ;;
     ;; Example:
     ;;
     ;; let a = f as (Int, Int)
     ;;   async -> Int
     ;; let b = t as (Int, Int)
     ;; async
     ;;   let c = 1
     ((equal (swift-mode:token:text next-token) "async")
      (equal (swift-mode:token:text (save-excursion
                                      (swift-mode:forward-token-simple)
                                      (swift-mode:forward-token-simple)))
             "let"))

     ;; After async
     ;;
     ;; Suppresses implicit semicolon if before let.
     ((and (equal (swift-mode:token:text previous-token) "async")
           (equal (swift-mode:token:text next-token) "let"))
      nil)

     ;; Suppress implicit semicolon around else
     ((or
       (equal (swift-mode:token:text previous-token) "else")
       (equal (swift-mode:token:text next-token) "else"))
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

     ;; Suppress implicit semicolon after attributes.
     ((eq (swift-mode:token:type previous-token) 'attribute)
      nil)

     ;; Suppress implicit semicolon after modifiers.
     ((member (swift-mode:token:text previous-token)
              '("indirect" "convenience" "dynamic" "final" "infix" "lazy"
                "mutating" "nonmutating" "optional" "override" "postfix"
                "prefix" "required" "static" "unowned" "weak" "internal"
                "package" "private" "public" "open" "fileprivate" "nonisolated"
                "distributed"))
      nil)

     ;; internal(set) private(set) public(set) open(set) fileprivate(set)
     ;; unowned(safe) unowned(unsafe) nonisolated(unsafe)
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
                    "fileprivate" "nonisolated")))))
      nil)

     ;; Suppress implicit semicolon after declaration starters.
     ((member (swift-mode:token:text previous-token)
              '("class" "struct" "actor" "protocol" "enum" "extension" "func"
                "typealias" "associatedtype" "precedencegroup" "operator"
                "macro"))
      nil)

     ;; Insert implicit semicolon before modifiers.
     ;;
     ;; Preceding modifiers takes precedence over this.
     ((member (swift-mode:token:text next-token)
              '("indirect" "convenience" "dynamic" "final" "infix" "lazy"
                "mutating" "nonmutating" "optional" "override" "postfix"
                "prefix" "required" "static" "unowned" "weak" "internal"
                "package" "private" "public" "open" "fileprivate" "nonisolated"
                "distributed"))
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

     ;; Suppress import semicolon after `repeat' unless followed by a open
     ;; curly bracket.
     ((and (equal (swift-mode:token:text previous-token) "repeat")
           (not (eq (swift-mode:token:type next-token) '{)))
      nil)

     ;; Inserts implicit semicolon before keywords that starts a new
     ;; statement.
     ((member (swift-mode:token:text next-token)
              '("for" "repeat" "case" "default" "defer" "do"
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

     ;; Inserts implicit semicolon before keywords that behave like method
     ;; names.
     ((member (swift-mode:token:text next-token)
              '("get" "set" "willSet" "didSet" "subscript" "init" "deinit"))
      t)

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
              '("class" "struct" "actor" "enum" "extension" "func" "typealias"
                "associatedtype" "precedencegroup" "macro"))
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

     ;; Inserts implicit semicolon before open parenthesis, unless it is a
     ;; function parameter clause. Suppress implicit semicolon before function
     ;; parameter clause.
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
     ((eq (swift-mode:token:type next-token) '\()
      (not (swift-mode:function-parameter-clause-p)))

     ;; Suppress implicit semicolon after the beginning of an interpolated
     ;; expression.
     ((eq (swift-mode:token:type previous-token)
          'string-chunk-before-interpolated-expression)
      nil)

     ;; Otherwise, inserts implicit semicolon.
     (t t))))

(defun swift-mode:function-parameter-clause-p ()
  "Return t if the cursor is before a function/macro parameter clause.

Return nil otherwise."
  (save-excursion
    (let* ((previous-token (swift-mode:backward-token-simple))
           (previous-type (swift-mode:token:type previous-token)))
      (cond
       ((eq previous-type '>)
        (and
         (/= (point)
             ;; FIXME: mutual dependency
             (progn (swift-mode:try-backward-generic-parameters) (point)))
         (swift-mode:function-parameter-clause-p)))
       ((eq previous-type 'identifier)
        (member (swift-mode:token:text (swift-mode:backward-token-simple))
                '("func" "macro")))
       (t nil)))))

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
               '("class" "extension" "enum" "struct" "actor" "protocol"
                 "typealias" "associatedtype"))))))

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
           ;;                        // actually returns t.
           ;;                    b: // This function should return t but it
           ;;                       // actually return nil.
           ;;   let y = a ? b : c // This function returns nil correctly for
           ;;                     // this.
           ;; }

           ;; FIXME: mutual dependency
           (swift-mode:token:text
            (swift-mode:backward-sexps-until
             '(implicit-\; \; { \( \[ "case" "default" ":")))
           '("case" "default"))
        (setq swift-mode:in-recursive-call-of-case-colon-p nil)))))

(defun swift-mode:anonymous-parameter-in-p ()
  "Return t if a `in' token at the cursor is for anonymous function parameters."
  (save-excursion
    (eq
     ;; FIXME: mutual dependency
     (swift-mode:token:type (swift-mode:backward-sexps-until
                             '(\; { \( \[ "for")))
     '{)))

(defun swift-mode:generic-parameter-clause-start-p ()
  "Return t if the `<' at the cursor is a start of generic parameters.

Return nil otherwise."
  (save-excursion
    (or (member (swift-mode:token:text (swift-mode:backward-token-simple))
                '("init" "subscript"))
        (member (swift-mode:token:text (swift-mode:backward-token-simple))
                '("typealias" "func" "enum" "struct" "actor" "class" "init"
                  "macro")))))

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
                             (memq (char-before start) '(?\( ?\[ ?{ ?, ?\; ?:))
                             (nth 4 (save-excursion
                                      (syntax-ppss (1- start))))))
       (has-following-space (or
                             (= end (point-max))
                             (memq (char-syntax (char-after end)) '(?  ?<))
                             (memq (char-after end) '(?\) ?\] ?} ?, ?\; ?:))
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
         ((member text '("try" "try?" "try!" "await" "consume" "copy"
                         "discard" "each"))
          'prefix-operator)
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
type `outside-of-buffer'."
  (let ((pos (point)))
    (let ((chunk (swift-mode:chunk-after)))
      (when (swift-mode:chunk:comment-p chunk)
        (goto-char (swift-mode:chunk:start chunk))))
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

     ;; Start of generic-parameter-clause
     ((and
       (eq (char-after) ?<)
       (swift-mode:generic-parameter-clause-start-p))
      (swift-mode:token '<
                        "<"
                        (progn (forward-char) (1- (point)))
                        (point)))

     (t
      (let ((token (swift-mode:forward-token-simple)))
        (setq token (swift-mode:backquote-identifier-if-after-dot token))

        (when (and (equal (swift-mode:token:text token) "in")
                   (save-excursion
                     (goto-char (swift-mode:token:start token))
                     (swift-mode:anonymous-parameter-in-p)))
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

   ;; End of interpolated expression
   ((and (eq (char-after) ?\))
         (equal (get-text-property (point) 'syntax-table)
                (string-to-syntax "|")))
    (let ((pos-after-comment (point)))
      (swift-mode:forward-string-chunk)
      (swift-mode:token
       'string-chunk-after-interpolated-expression
       (buffer-substring-no-properties pos-after-comment (point))
       pos-after-comment
       (point))))

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
   ;; for angle bracket, and a type parameter starts with an upper case
   ;; character, a square bracket, a parenthesis, or keyword 'protocol'.
   ((and (eq (char-after) ?<)
         (looking-at "<\\([[:upper:][(]\\|protocol\\)"))
    (forward-char)
    (swift-mode:token '< "<" (1- (point)) (point)))

   ;; Close angle bracket for type parameters
   ;;
   ;; Close angle bracket follows identifier, a square bracket, a parenthesis,
   ;; or another angle bracket (e.g. Foo<Bar<[(Int, String)]>>)
   ((and (eq (char-after) ?>)
         (save-excursion
           ;; You know that regular languages can be reversed. Thus you may
           ;; think that `looking-back' reverses the given regexp and scans
           ;; chars backwards.  Nevertheless, `looking-back' function does not
           ;; do that.  It just repeats `looking-at' with decrementing start
           ;; position until it succeeds. The document says that it is not
           ;; recommended to use.  So using `skip-chars-backward',
           ;; `skip-syntax-backward', and `looking-at' here.
           (skip-chars-backward "])>")
           (skip-syntax-backward "w_")
           (looking-at "[[:upper:]_]")))
    (forward-char)
    (swift-mode:token '> ">" (1- (point)) (point)))

   ;; Regex
   ((and (looking-at "#*/")
         (equal (get-text-property (match-beginning 0) 'syntax-table)
                (string-to-syntax "|")))
    (let ((pos-after-comment (point)))
      (swift-mode:forward-string-chunk)
      (swift-mode:token
       'identifier
       (buffer-substring-no-properties pos-after-comment (point))
       pos-after-comment
       (point))))

   ;; Operator (other than as, try, is, await, consume, copy, discard, or each)
   ;;
   ;; Operators starts with a dot can contains dots. Other operators cannot
   ;; contain dots.
   ;;
   ;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/swift/grammar/dot-operator-head
   ;; TODO Unicode operators
   ((looking-at "[-/=+!*%<>&|^~?]+\\|[.][-./=+!*%<>&|^~?]*")
    (let* ((text (match-string-no-properties 0))
           (start (match-beginning 0))
           (end (match-end 0)))
      (when (string-match "^.*?/\\*\\|^.*?//" text)
        ;; e.g. +++/* */ or +++//
        (setq end (- end (- (length text) (- (match-end 0) 2))))
        (setq text (substring text 0 (- (match-end 0) 2))))
      (when (and (string-match "^.*?/" text)
                 (equal (get-text-property (+ start (1- (match-end 0)))
                                           'syntax-table)
                        (string-to-syntax "|")))
        ;; Regex after prefix operator, e.g. +++/<>/
        (setq end (- end (- (length text) (- (match-end 0) 1))))
        (setq text (substring text 0 (- (match-end 0) 1))))
      (goto-char end)
      (swift-mode:fix-operator-type
       (swift-mode:token nil text start end))))

   ;; Backquoted identifier
   ((eq (char-after) ?`)
    (let ((pos-after-comment (point)))
      (swift-mode:forward-string-chunk)
      (swift-mode:token
       'identifier
       (buffer-substring-no-properties pos-after-comment (point))
       pos-after-comment
       (point))))

   ;; String
   ((looking-at "#*\"")
    (let ((pos-after-comment (point)))
      (skip-chars-forward "#")
      (forward-char)
      (swift-mode:end-of-string)
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

   ;; Other tokens including identifiers, implicit parameters, keywords, and
   ;; numbers
   (t
    (let*
        ((pos-after-comment (point))
         (text
          (cond
           ;; Identifiers, implicit parameters, keywords, numbers
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
       ((member text '("await" "consume" "copy" "discard" "each"))
        (swift-mode:token 'prefix-operator
                          text
                          (- (point) (length text))
                          (point)))
       (t
        (swift-mode:token 'identifier
                          text
                          (- (point) (length text))
                          (point))))))))

(defun swift-mode:backward-token ()
  "Move point backward to the previous position of the end of a token.

Return the token object.  If no more tokens available, return a token with
type `outside-of-buffer'."

  (let ((pos (point)))
    (let ((chunk (swift-mode:chunk-after)))
      (when (swift-mode:chunk:comment-p chunk)
        (goto-char (swift-mode:chunk:start chunk))))
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

     ;; Start of generic-parameter-clause
     ((and
       (eq (char-before) ?<)
       (save-excursion
         (backward-char)
         (swift-mode:generic-parameter-clause-start-p)))
      (backward-char)
      (swift-mode:token '<
                        "<"
                        (point)
                        (1+ (point))))

     (t
      (let ((token (swift-mode:backward-token-simple)))
        (setq token (swift-mode:backquote-identifier-if-after-dot token))

        (when (and (equal (swift-mode:token:text token) "in")
                   (save-excursion
                     (goto-char (swift-mode:token:start token))
                     (swift-mode:anonymous-parameter-in-p)))
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

   ;; Beginning of interpolated expression
   ((and (eq (char-before) ?\()
         (equal (get-text-property (1- (point)) 'syntax-table)
                (string-to-syntax "|")))
    (let ((pos-before-comment (point)))
      (swift-mode:backward-string-chunk)
      (swift-mode:token
       'string-chunk-before-interpolated-expression
       (buffer-substring-no-properties (point) pos-before-comment)
       (point)
       pos-before-comment)))

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
   ;; for angle bracket, and a type parameter starts with an upper case
   ;; character, a square bracket, a parenthesis, or keyword `protocol'.
   ((and (eq (char-before) ?<)
         (looking-at "\\([[:upper:][(]\\|protocol\\)"))
    (backward-char)
    (swift-mode:token '< "<" (point) (1+ (point))))

   ;; Close angle bracket for type parameters
   ;;
   ;; Close angle bracket follows identifier, a square bracket, a parenthesis,
   ;; or another angle bracket (e.g. Foo<Bar<[(Int, String)]>>)
   ((and (eq (char-before) ?>)
         (save-excursion
           (skip-chars-backward "])>")
           (skip-syntax-backward "w_")
           (looking-at "[[:upper:]_]")))
    (backward-char)
    (swift-mode:token '> ">" (point) (1+ (point))))

   ;; Regex
   ((and (save-excursion
           (skip-chars-backward "#")
           (eq (char-before) ?/))
         (equal (get-text-property (1- (point)) 'syntax-table)
                (string-to-syntax "|")))
    (let ((pos-before-comment (point)))
      (swift-mode:backward-string-chunk)
      (swift-mode:token
       'identifier
       (buffer-substring-no-properties (point) pos-before-comment)
       (point)
       pos-before-comment)))

   ;; Operator (other than as, try, is, await, consume, copy, discard, or each)
   ;;
   ;; Operators which starts with a dot can contain other dots. Other
   ;; operators cannot contain dots.
   ;;
   ;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/swift/grammar/dot-operator-head
   ;; TODO Unicode operators
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

   ;; Backquoted identifier
   ((eq (char-before) ?`)
    (let ((pos-before-comment (point)))
      (swift-mode:backward-string-chunk)
      (swift-mode:token
       'identifier
       (buffer-substring-no-properties (point) pos-before-comment)
       (point)
       pos-before-comment)))

   ;; String
   ((save-excursion
      (skip-chars-backward "#")
      (eq (char-before) ?\"))
    (let ((pos-before-comment (point)))
      (skip-chars-backward "#")
      (backward-char)
      (swift-mode:beginning-of-string)
      (swift-mode:token
       'identifier
       (buffer-substring-no-properties (point) pos-before-comment)
       (point)
       pos-before-comment)))

   ;; Other tokens including identifiers, implicit parameters, keywords, and
   ;; numbers
   (t
    (let*
        ((pos-before-comment (point))
         (text
          (cond
           ;; Identifiers, implicit parameters, keywords, numbers
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
       ((member text '("try" "await" "consume" "copy" "discard" "each"))
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

(defun swift-mode:forward-string-chunk ()
  "Skip forward a string chunk.

A string chunk is a part of single-line/multiline string delimited with
quotation marks or interpolated expressions."
  (condition-case nil
      (goto-char (scan-sexps (point) 1))
    (scan-error (goto-char (point-max)))))

(defun swift-mode:backward-string-chunk ()
  "Skip backward a string chunk.

A string chunk is a part of single-line/multiline string delimited with
quotation marks or interpolated expressions."
  (condition-case nil
      (goto-char (scan-sexps (point) -1))
    (scan-error (goto-char (point-min)))))

(defun swift-mode:beginning-of-string ()
  "Move point to the beginning of single-line/multiline string.

Assuming the cursor is on a string."
  (goto-char (or (nth 8 (syntax-ppss)) (point)))
  (let (matching-parenthesis)
    (while (setq matching-parenthesis
                 (get-text-property
                  (point)
                  'swift-mode:matching-parenthesis))
      (goto-char matching-parenthesis)
      (goto-char (nth 8 (syntax-ppss))))
    (point)))

(defun swift-mode:end-of-string ()
  "Move point to the end of single-line/multiline string.

Assuming the cursor is on a string."
  (goto-char (or (nth 8 (syntax-ppss)) (point)))
  (let (matching-parenthesis)
    (swift-mode:forward-string-chunk)
    (while (setq matching-parenthesis
                 (get-text-property
                  (1- (point))
                  'swift-mode:matching-parenthesis))
      (goto-char matching-parenthesis)
      (swift-mode:forward-string-chunk)))
  (point))

(defun swift-mode:goto-non-comment-bol ()
  "Back to the beginning of line that is not inside a comment."
  (forward-line 0)
  (let (chunk)
    (while (progn
             (setq chunk (swift-mode:chunk-after))
             (swift-mode:chunk:comment-p chunk))
      ;; The cursor is in a comment. Backs to the beginning of the comment.
      (goto-char (swift-mode:chunk:start chunk))
      (forward-line 0))))

(defun swift-mode:goto-non-comment-eol ()
  "Proceed to the end of line that is not inside a comment.

If this line ends with a single-line comment, goto just before the comment."
  (end-of-line)
  (let (chunk)
    (while (progn
             (setq chunk (swift-mode:chunk-after))
             (swift-mode:chunk:comment-p chunk))
      ;; The cursor is in a comment.
      (if (swift-mode:chunk:single-line-comment-p chunk)
          ;; This is a single-line comment
          ;; Back to the beginning of the comment.
          (goto-char (swift-mode:chunk:start chunk))
        ;; This is a multiline comment
        ;; Proceed to the end of the comment.
        (goto-char (swift-mode:chunk:start chunk))
        (forward-comment 1)
        (end-of-line)
        (when (and (eobp) (swift-mode:chunk-after))
          (goto-char (swift-mode:chunk:start (swift-mode:chunk-after))))))))

;;; Comment or string chunks

;; A chunk is either a string-chunk, regex, or a comment.
;; It have the type and the start position.

(defun swift-mode:chunk (type start)
  "Return a new chunk with TYPE and START position."
  (list type start))

(defun swift-mode:chunk:type (chunk)
  "Return the type of the CHUNK."
  (nth 0 chunk))

(defun swift-mode:chunk:start (chunk)
  "Return the start position of the CHUNK."
  (nth 1 chunk))

(defun swift-mode:chunk:end (chunk)
  "Return the end position of the CHUNK."
  (save-excursion
    (goto-char (nth 1 chunk))
    (if (swift-mode:chunk:comment-p chunk)
        (forward-comment 1)
      (swift-mode:forward-token))
    (point)))

(defun swift-mode:chunk:comment-p (chunk)
  "Return non-nil if the CHUNK is a comment."
  (memq (swift-mode:chunk:type chunk) '(single-line-comment multiline-comment)))

(defun swift-mode:chunk:string-p (chunk)
  "Return non-nil if the CHUNK is a string."
  (memq (swift-mode:chunk:type chunk) '(single-line-string multiline-string)))

(defun swift-mode:chunk:single-line-comment-p (chunk)
  "Return non-nil if the CHUNK is a single-line comment."
  (eq (swift-mode:chunk:type chunk) 'single-line-comment))

(defun swift-mode:chunk:multiline-comment-p (chunk)
  "Return non-nil if the CHUNK is a multiline comment."
  (eq (swift-mode:chunk:type chunk) 'multiline-comment))

(defun swift-mode:chunk:single-line-string-p (chunk)
  "Return non-nil if the CHUNK is a single-line string."
  (eq (swift-mode:chunk:type chunk) 'single-line-string))

(defun swift-mode:chunk:multiline-string-p (chunk)
  "Return non-nil if the CHUNK is a multiline string."
  (eq (swift-mode:chunk:type chunk) 'multiline-string))

(defun swift-mode:chunk:regex-p (chunk)
  "Return non-nil if the CHUNK is a regex."
  (eq (swift-mode:chunk:type chunk) 'regex))

(defun swift-mode:chunk:pound-count (chunk)
  "Return the number of pound signs before the start position of the CHUNK."
  (save-excursion
    (goto-char (swift-mode:chunk:start chunk))
    (swift-mode:beginning-of-string)
    (skip-chars-forward "#")
    (- (swift-mode:chunk:start chunk) (point))))

(defun swift-mode:chunk-after (&optional parser-state)
  "Return the chunk at the cursor.

If the cursor is outside of strings and comments, return nil.

If PARSER-STATE is given, it is used instead of (syntax-ppss)."
  (save-excursion
    (when (number-or-marker-p parser-state)
      (goto-char parser-state))
    (when (or (null parser-state) (number-or-marker-p parser-state))
      (setq parser-state (save-excursion (syntax-ppss parser-state))))
    (cond
     ((nth 3 parser-state)
      ;; Syntax category "|" is attached to both single-line and multiline
      ;; string delimiters.  So (nth 3 parser-state) may be t even for
      ;; single-line string delimiters.
      (cond
       ((save-excursion (goto-char (nth 8 parser-state))
                        (looking-at "#*\"\"\""))
        (swift-mode:chunk 'multiline-string (nth 8 parser-state)))
       ((save-excursion (goto-char (nth 8 parser-state))
                        (looking-at "#*/"))
        (swift-mode:chunk 'regex (nth 8 parser-state)))
       (t (swift-mode:chunk 'single-line-string (nth 8 parser-state)))))

     ((eq (nth 4 parser-state) t)
      (swift-mode:chunk 'single-line-comment (nth 8 parser-state)))

     ((nth 4 parser-state)
      (swift-mode:chunk 'multiline-comment (nth 8 parser-state)))

     ((and (eq (char-before) ?/) (eq (char-after) ?/))
      (swift-mode:chunk 'single-line-comment (1- (point))))

     ((and (eq (char-before) ?/) (eq (char-after) ?*))
      (swift-mode:chunk 'multiline-comment (1- (point))))

     (t
      nil))))

(defun swift-mode:same-line-p (point1 point2)
  "Return non-nil if POINT1 and POINT2 is on the same line.

Return nil otherwise."
  (= (save-excursion
       (goto-char point1)
       (line-beginning-position))
     (save-excursion
       (goto-char point2)
       (line-beginning-position))))

(provide 'swift-mode-lexer)

;;; swift-mode-lexer.el ends here
