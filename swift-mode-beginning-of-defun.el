;;; swift-mode-beginning-of-defun.el --- Major-mode for Apple's Swift programming language, beginning/end-of-defun. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2018 taku0

;; Authors: taku0 (http://github.com/taku0)
;;
;; Version: 7.1.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
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

;; `beginning-of-defun' and `end-of-defun'
;;
;; A defun is a declaration except local variable, "get", "set", "willSet",
;; "didSet", or "case" within enum.
;;
;; A defun include modifiers, attributes, and comments on the same line.
;;
;; `swift-mode:beginning-of-defun' moves the point to the beginning of a defun
;; that precedes (if the arg is positive) or follows (if the arg is negative)
;; the original point and has the same or less nesting level.
;;
;; `swift-mode:end-of-defun' moves the point to the end of a defun
;; that follows (if the arg is positive) or precedes (if the arg is negative)
;; the original point and has the same or less nesting level.

;;; Code:

(require 'swift-mode-lexer)
(require 'swift-mode-indent)

(defcustom swift-mode:mark-defun-preference 'containing
  "Preference for `swift-mode:mark-defun' for nested declarations.

Suppose the following code with the point located at A:

    func outer() {
      func inner1() {
      }

      // A

      func inner2() {
      }
    }

If `swift-mode:mark-defun-preference' is `containing', `swift-mode:mark-defun'
marks the `outer' function.  Likewise, it marks `inner1' if the preference is
`preceding' and `inner2' if the preference is `following'."
  :type '(choice (const :tag "Containing" containing)
                 (const :tag "Preceding" preceding)
                 (const :tag "Following" following))
  :group 'swift
  :safe 'symbolp)

(defvar swift-mode:last-mark-direction 'containing
  "Last direction of `swift-mode:mark-generic-block'.")

(defun swift-mode:beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.

See `beginning-of-defun' for ARG.

Return t if a defun is found.  Return nil otherwise.

Push mark at previous position if this is called as a command, not repeatedly,
and the region is not active."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((result t)
        (pos (point)))
    (if (< 0 arg)
        ;; Moving backward
        (while (and result (< 0 arg))
          (let ((last-position (point)))
            (setq result (swift-mode:beginning-of-defun-backward))
            (when (< (point) last-position)
              (setq arg (1- arg)))
            (when (< 0 arg)
              (swift-mode:backward-token-or-list))))
      ;; Moving forward
      (setq result (swift-mode:beginning-of-defun-forward))
      (when (and result (< pos (point)))
        (setq arg (1+ arg)))
      (while (and result (< arg 0))
        (swift-mode:forward-statement)
        (forward-comment (point-max))
        (setq result (swift-mode:beginning-of-defun-forward))
        (setq arg (1+ arg))))
    (and result
         (eq this-command 'swift-mode:beginning-of-defun)
         (not (eq last-command 'swift-mode:beginning-of-defun))
         (not (region-active-p))
         (push-mark pos))
    result))

(defun swift-mode:beginning-of-defun-backward ()
  "Goto the beginning of a defun at or before the cursor."
  (let ((keyword-token nil))
    (while (null keyword-token)
      (swift-mode:beginning-of-statement)
      (setq keyword-token (swift-mode:find-defun-keyword))
      (unless keyword-token
        (let ((previous-token (swift-mode:backward-token-or-list)))
          (when (eq (swift-mode:token:type previous-token) 'outside-of-buffer)
            (setq keyword-token previous-token)))))
    (not (eq (swift-mode:token:type keyword-token) 'outside-of-buffer))))

(defun swift-mode:beginning-of-defun-forward ()
  "Goto the beginning of a defun at or after the cursor.

If the cursor is not at the beginning of a statement, the cursor may go back to
the beginning of the current statement."
  (let ((keyword-token nil))
    (while (null keyword-token)
      (setq keyword-token (swift-mode:find-defun-keyword))
      (if keyword-token
          (progn
            (goto-char (swift-mode:token:start keyword-token))
            (swift-mode:beginning-of-statement))
        (let ((last-token (swift-mode:forward-statement)))
          (when (eq (swift-mode:token:type last-token) 'outside-of-buffer)
            (setq keyword-token last-token))
          (forward-comment (point-max)))))
    (not (eq (swift-mode:token:type keyword-token) 'outside-of-buffer))))

(defun swift-mode:find-defun-keyword ()
  "Find a defun keyword token in the current statement.

If a keyword found in the current statement, return the token.
Otherwise, return nil.
The cursor must be at the beginning of a statement."
  (save-excursion
    (let ((token (swift-mode:find-defun-keyword-simple)))
      (cond
       ((member (swift-mode:token:text token) '("var" "let"))
        (when (swift-mode:class-like-member-p) token))
       ((equal (swift-mode:token:text token) "case")
        (swift-mode:backward-sexps-until '({))
        (swift-mode:beginning-of-statement)
        (let ((parent-token (swift-mode:find-defun-keyword-simple)))
          (when (equal (swift-mode:token:text parent-token) "enum")
            token)))
       (t token)))))

(defun swift-mode:find-defun-keyword-simple ()
  "Find a defun keyword token in the current statement.

If a keyword found in the current statement, return the token.
Return the token for local variable declarations as well.
Otherwise, return nil.
The cursor must be at the beginning of a statement."
  (let ((token (swift-mode:forward-token-or-list))
        (defun-keywords
          '("import" "typealias" "associatedtype"
            "enum" "struct" "protocol" "extension"
            "func" "init" "deinit" "subscript" "get" "set" "willSet" "didSet"
            "prefix" "postfix" "infix" "precedencegroup"
            "var" "let"
            "case"))
        (stop-tokens '(\; implicit-\; {} { } \( \) \[ \]
                       anonymous-function-parameter-in outside-of-buffer))
        (class-token nil))
    (while (not (or
                 (memq (swift-mode:token:type token) stop-tokens)
                 (member (swift-mode:token:text token) defun-keywords)))
      ;; "class" token may be either a class declaration keyword or a modifier:
      ;;
      ;; // Nested class named "final"
      ;; class Foo { class final {} }
      ;;
      ;; // Non-overridable class method named "foo"
      ;; class Foo { class final func foo() {} }
      ;;
      ;; Keeps scanning and returns the token if there are no other
      ;; `defun-keywords'.
      (when (equal (swift-mode:token:text token) "class")
        (setq class-token token))
      (setq token (swift-mode:forward-token-or-list)))
    (if (member (swift-mode:token:text token) defun-keywords)
        token
      class-token)))

(defun swift-mode:class-like-member-p ()
  "Return t if the cursor is on a member of a class-like declaration.
Also return t if the cursor is on a global declaration.
Return nil otherwise."
  (or
   (let ((parent (swift-mode:backward-sexps-until '({))))
     (eq (swift-mode:token:type parent) 'outside-of-buffer))
   (progn
     (swift-mode:beginning-of-statement)
     (member
      (swift-mode:token:text (swift-mode:find-defun-keyword-simple))
      '("enum" "struct" "class" "protocol" "extension")))))

(defun swift-mode:beginning-of-statement ()
  "Move backward to the beginning of a statement.
Statements include comments on the same line.

When called at the beginning of a statement, keep the position.

Intended for internal use."
  (let ((chunk (swift-mode:chunk-after)))
    (when chunk
      (goto-char (swift-mode:chunk:start chunk))))
  (when (and (eq (char-syntax (or (char-after) ?.)) ?w)
             (eq (char-syntax (or (char-before) ?.)) ?w))
    (swift-mode:forward-token))
  (let ((pos (point))
        (previous-token (save-excursion
                          (forward-comment (- (point)))
                          (swift-mode:backward-token))))
    (forward-comment (point-max))
    (swift-mode:goto-non-comment-bol)
    (swift-mode:skip-whitespaces)

    ;; We have three cases:
    ;;
    ;; func foo() {
    ;; }
    ;;
    ;; // A
    ;;
    ;; /* B */ func /* C */ bar() {
    ;; }
    (cond
     ((or (< pos (point))
          (memq (swift-mode:token:type
                 (save-excursion (swift-mode:forward-token)))
                '(} \) \])))
      ;; The pos is at A or just before closing parens.
      (if (memq (swift-mode:token:type previous-token)
                swift-mode:statement-parent-tokens)
          ;; At beginning of a block. Goes up.
          (goto-char (swift-mode:token:start previous-token))
        ;; Otherwise, skips implicit semicolons.
        (goto-char (swift-mode:token:end previous-token)))
      (forward-comment (- (point)))
      (swift-mode:do-beginning-of-statement)
      (when (< pos (point))
        ;; no statements found
        (goto-char (point-min))))
     ((< (point) (swift-mode:token:end previous-token))
      ;; The pos is at C.
      (goto-char (swift-mode:token:end previous-token))
      (swift-mode:do-beginning-of-statement))
     (t
      ;; The pos is at B.
      (forward-comment (point-max))
      (swift-mode:do-beginning-of-statement)))))

(defun swift-mode:do-beginning-of-statement ()
  "Move backward to the beginning of a statement.
Statements include comments on the same line.

Intended for internal use."
  (let (parent)
    (while (progn
             (setq parent (swift-mode:backward-sexps-until
                           swift-mode:statement-parent-tokens))
             (swift-mode:pseudo-implicit-semicolon-p parent)))
    (goto-char (swift-mode:token:end parent))
    ;; Excludes comments on previous lines but includes comments on the same
    ;; line.
    (forward-comment (- (point)))
    (setq parent (save-excursion (swift-mode:backward-token-or-list)))
    (forward-comment (point-max))
    (swift-mode:goto-non-comment-bol)
    (when (< (point) (swift-mode:token:end parent))
      (goto-char (swift-mode:token:end parent)))
    (swift-mode:skip-whitespaces)))

(defun swift-mode:backward-statement ()
  "Move backward to the beginning of a statement.
Statements include comments on the same line.

Intended for internal use."
  (let ((pos (point)))
    (swift-mode:beginning-of-statement)
    (when (<= pos (point))
      (swift-mode:backward-token-or-list)
      (swift-mode:beginning-of-statement))))

(defun swift-mode:end-of-statement ()
  "Move forward to the end of a statement.

When called at the end of a sentence, keep the position.

Return the next token.
Intended for internal use."
  (let ((chunk (swift-mode:chunk-after)))
    (when chunk
      (goto-char (swift-mode:chunk:start chunk))
      (forward-comment 1)))
  (let ((pos (point))
        (previous-token (save-excursion (swift-mode:backward-token)))
        next-token)
    (cond
     ((and
       (memq (swift-mode:token:type previous-token)
             '(\; anonymous-function-parameter-in))
       (eq (swift-mode:token:end previous-token) pos))
      ;; Already at the end of statement.  Returns next token.
      (save-excursion (swift-mode:forward-token)))
     ((memq (swift-mode:token:type previous-token)
            '(implicit-\; outside-of-buffer))
      ;; Between statements, or before the first statement.
      (swift-mode:forward-statement))
     ((progn
        (setq next-token (save-excursion (swift-mode:forward-token)))
        (and (memq (swift-mode:token:type next-token)
                   '(implicit-\; } outside-of-buffer))
             (eq (swift-mode:token:end previous-token) pos)))
      ;; Already at the end of statement.  Returns next token.
      next-token)
     (t
      (swift-mode:forward-statement)))))

(defun swift-mode:forward-statement ()
  "Move forward to the end of a statement.

Return the next token.
Intended for internal use."
  (let ((chunk (swift-mode:chunk-after)))
    (when chunk
      (goto-char (swift-mode:chunk:start chunk))))
  (when (and (eq (char-syntax (or (char-after) ?.)) ?w)
             (eq (char-syntax (or (char-before) ?.)) ?w))
    (swift-mode:backward-token))
  (forward-comment (point-max))
  (let ((pos (point))
        token)
    (while (progn
             (setq token (swift-mode:forward-token-or-list))
             (or
              (not (memq (swift-mode:token:type token)
                         '(\; implicit-\; } anonymous-function-parameter-in
                           outside-of-buffer)))
              (swift-mode:pseudo-implicit-semicolon-p token))))
    (if (memq (swift-mode:token:type token)
              '(\; anonymous-function-parameter-in))
        (goto-char (swift-mode:token:end token))
      (goto-char (swift-mode:token:start token)))
    (while (eq (swift-mode:token:type
                (save-excursion (swift-mode:forward-token)))
               '\;)
      (setq token (swift-mode:forward-token)))
    (cond
     ((eq (swift-mode:token:type token) 'outside-of-buffer)
      (forward-comment (- (point)))
      (when (<= (point) pos)
        (goto-char (swift-mode:token:end token)))
      token)
     ((eq (swift-mode:token:type token) '})
      (forward-comment (- (point)))
      (if (<= (point) pos)
          (progn
            (goto-char (swift-mode:token:end token))
            (swift-mode:end-of-statement))
        token))
     (t token))))

(defun swift-mode:end-of-defun (&optional arg)
  "Move forward to the end of a defun.

See `end-of-defun' for ARG.

Return t if a defun is found.  Return nil otherwise.

Push mark at previous position if this is called as a command, not repeatedly,
and the region is not active."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((result t)
        (pos (point))
        next-token)
    (if (<= 0 arg)
        ;; Moving forward
        (while (and result (< 0 arg))
          (setq next-token (swift-mode:forward-statement))
          (when (save-excursion
                  (swift-mode:beginning-of-statement)
                  (swift-mode:find-defun-keyword))
            (setq arg (1- arg)))
          (when (and (< 0 arg)
                     (eq (swift-mode:token:type next-token) 'outside-of-buffer))
            (setq result nil)))
      ;; Moving backward
      (while (and result (< arg 0))
        (setq result (swift-mode:end-of-statement-backward))
        (let ((statement-end-position (point)))
          (swift-mode:beginning-of-statement)
          (when (swift-mode:find-defun-keyword)
            (setq arg (1+ arg)))
          (when (eq arg 0)
            (goto-char statement-end-position)))))
    (and result
         (eq this-command 'swift-mode:end-of-defun)
         (not (eq last-command 'swift-mode:end-of-defun))
         (not (region-active-p))
         (push-mark pos))
    result))

(defun swift-mode:end-of-statement-backward ()
  "Move backward to the end of a statement.

Return t if a statement found.  Return nil otherwise.
When called at the end of a statement, find the previous one.
Intended for internal use."
  (when (save-excursion
          (let ((pos (point))
                (token (swift-mode:backward-token-or-list)))
            (and
             (memq (swift-mode:token:type token)
                   '(\; anonymous-function-parameter-in))
             (eq (swift-mode:token:end token) pos))))
    (swift-mode:backward-token))
  (or
   ;; last statement in non-empty block
   (and
    (let ((next-token (save-excursion (swift-mode:forward-token))))
      (memq (swift-mode:token:type next-token) '(} outside-of-buffer)))
    (let ((previous-token (save-excursion
                            (forward-comment (- (point)))
                            (swift-mode:backward-token))))
      (not (eq (swift-mode:token:type previous-token) '{)))
    (let ((pos (point)))
      (forward-comment (- (point)))
      (< (point) pos)))
   ;; other cases
   (let (token)
     (while (progn
              (setq token (swift-mode:backward-sexps-until
                           '(\; implicit-\; anonymous-function-parameter-in)))
              (swift-mode:pseudo-implicit-semicolon-p token)))
     (when (memq (swift-mode:token:type token)
                 '(\; anonymous-function-parameter-in))
       (goto-char (swift-mode:token:end token)))
     (not (eq (swift-mode:token:type token) 'outside-of-buffer)))))

(defun swift-mode:pseudo-implicit-semicolon-p (token)
  "Return t if TOKEN is an implicit semicolon not at end of a statement.

Return nil otherwise."
  ;; func foo() // implicit semicolon here
  ;; {
  ;; }
  ;;
  ;; if foo {
  ;; } // implicit semicolon here
  ;; else {
  ;; }
  ;;
  ;; do {
  ;; } // implicit semicolon here
  ;; catch {
  ;; }
  (and
   (eq (swift-mode:token:type token) 'implicit-\;)
   (save-excursion
     (goto-char (swift-mode:token:end token))
     (let ((next-token (swift-mode:forward-token)))
       (or
        (eq (swift-mode:token:type next-token) '{)
        (member (swift-mode:token:text next-token) '("catch" "else")))))))

(defun swift-mode:mark-defun (&optional arg allow-extend)
  "Put mark at the end of defun, point at the beginning of defun.

If the point is between defuns, mark depend on
`swift-mode:mark-defun-preference'.

If ARG is a positive number, mark that many following defuns.  If ARG is
negative, reverse direction of marking.  If those defuns have lesser nesting
level than the initial one, mark the whole outer defun.

If ALLOW-EXTEND is non-nil or called interactively, and the command is repeated
or the region is active, mark the following (if the point is before the mark)
or preceding (if the point is after the mark) defun."
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (let ((region (swift-mode:mark-generic-block
                 arg
                 allow-extend
                 #'swift-mode:end-of-defun
                 #'swift-mode:beginning-of-defun)))
    (if (and  (not region) (called-interactively-p 'interactive))
        (progn (message "No defun found") nil)
      region)))

(defun swift-mode:narrow-to-defun (&optional include-comments)
  "Make text outside current defun invisible.

If the point is between defuns, narrow depend on
`swift-mode:mark-defun-preference'.

Preceding comments are included if INCLUDE-COMMENTS is non-nil.
Interactively, the behavior depends on ‘narrow-to-defun-include-comments’."
  (interactive (list (and (boundp 'narrow-to-defun-include-comments)
                          narrow-to-defun-include-comments)))
  (let ((region (swift-mode:narrow-to-generic-block
                 include-comments
                 #'swift-mode:end-of-defun
                 #'swift-mode:beginning-of-defun)))
    (if (and  (not region) (called-interactively-p 'interactive))
        (progn (message "No defun found") nil)
      region)))

(defun swift-mode:forward-sentence (&optional arg)
  "Skip forward sentences or statements.

In comments or strings, skip a sentence.  Otherwise, skip a statement.

With ARG, repeat ARG times.  If ARG is negative, Skip backwards.

Return t if a sentence is found.  Return nil otherwise."
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (swift-mode:backward-sentence (- arg))
    (let ((result t))
      (while (and result (< 0 arg))
        (setq result (swift-mode:forward-sentence-1))
        (setq arg (1- arg)))
      result)))

(defun swift-mode:mark-generic-block (arg
                                      allow-extend
                                      move-forward
                                      move-backward)
  "Put mark at the end of generic block, point at the beginning of it.

The direction of marking depend on `swift-mode:mark-defun-preference'.

If ARG is a positive number, mark that many blocks.  If ARG is negative,
reverse direction of marking.  If those blocks have lesser nesting level than
the initial one, mark the whole outer block.

If ALLOW-EXTEND is non-nil or called interactively, and the command is repeated
or the region is active, extend region.

MOVE-FORWARD is a function moving the cursor to the next end of block.
MOVE-BACKWARD is a function moving the cursor to the previous beginning of
block.
Both functions return t if succeeded, return nil otherwise."
  (setq arg (or arg 1))

  (let ((reversed (< arg 0))
        (count (abs arg))
        (direction
         (if (and allow-extend
                  (and (eq last-command this-command) (mark t)))
             swift-mode:last-mark-direction
           swift-mode:mark-defun-preference))
        (original-region
         (if (and allow-extend
                  (or
                   (and (eq last-command this-command) (mark t))
                   (region-active-p)))
             (cons (min (point) (mark t))
                   (max (point) (mark t)))
           (cons (point) (point))))
        (point-was-after-mark
         (and (mark t)
              (< (mark t) (point))))
        new-region
        new-direction
        last-successful-region)
    (when reversed
      (setq direction
            (cond
             ((eq direction 'containing) 'containing)
             ((eq direction 'preceding) 'following)
             ((eq direction 'following) 'preceding))))

    (setq new-region original-region)
    (setq new-direction direction)

    (while (and new-region (< 0 count))
      (let ((new-region-and-direction
             (swift-mode:extend-region-to-be-marked
              new-region
              new-direction
              move-forward
              move-backward
              (if reversed 'preceding 'following))))
        (setq new-region (nth 0 new-region-and-direction))
        (setq new-direction (nth 1 new-region-and-direction)))
      (when new-region
        (setq last-successful-region new-region))
      (setq count (1- count)))

    (setq new-region (or new-region last-successful-region))
    (setq swift-mode:last-mark-direction new-direction)

    (and
     new-region
     (progn
       (goto-char (car new-region))
       (push-mark (cdr new-region) nil t)

       (if (eq (car original-region) (cdr original-region))
           (when (eq new-direction 'preceding)
             (exchange-point-and-mark))
         (when point-was-after-mark
           (exchange-point-and-mark)))
       new-region))))

(defun swift-mode:extend-region-to-be-marked (original-region
                                              direction
                                              move-forward
                                              move-backward
                                              preferred-direction)
  "Return cons representing the extended region.

ORIGINAL-REGION is the region to be extended.
DIRECTION is the direction of extension.
MOVE-FORWARD is a function moving the cursor to the next end of block.
MOVE-BACKWARD is a function moving the cursor to the previous beginning of
block.
Both functions return t if succeeded, return nil otherwise.
PREFERRED-DIRECTION is the preferred direction of extension when DIRECTION is
 `containing'."
  (let* ((new-region-and-direction
          (cond
           ((eq direction 'containing)
            (swift-mode:containing-generic-block-region
             original-region
             move-forward move-backward
             preferred-direction))
           ((eq direction 'preceding)
            (list
             (save-excursion
               (goto-char (car original-region))
               (swift-mode:preceding-generic-block-region
                move-forward move-backward))
             'preceding))
           ((eq direction 'following)
            (list
             (save-excursion
               (goto-char (cdr original-region))
               (swift-mode:following-generic-block-region
                move-forward move-backward))
             'following))))
         (new-region (nth 0 new-region-and-direction))
         (new-direction (nth 1 new-region-and-direction)))
    (when new-region
      (when (/= (car original-region) (cdr original-region))
        (setq new-region
              (cons
               (min (car original-region) (car new-region))
               (max (cdr original-region) (cdr new-region)))))
      ;; Marks the whole outer block if the mark got out of the outer block.
      (save-excursion
        (goto-char (cdr new-region))
        (funcall move-backward)
        (setcar new-region (min (car new-region) (point))))
      (save-excursion
        (goto-char (car new-region))
        (funcall move-forward)
        (setcdr new-region (max (cdr new-region) (point)))))
    (list new-region new-direction)))

(defun swift-mode:following-generic-block-region (move-forward move-backward)
  "Return cons representing a region of following generic block.

MOVE-FORWARD is a function moving the cursor to the next end of block.
MOVE-BACKWARD is a function moving the cursor to the previous beginning of
block.
Both functions return t if succeeded, return nil otherwise."
  (save-excursion
    (let* ((end (and (funcall move-forward) (point)))
           (beginning (and end (funcall move-backward) (point))))
      (and beginning (cons beginning end)))))

(defun swift-mode:preceding-generic-block-region (move-forward move-backward)
  "Return cons representing a region of preceding generic block.

MOVE-FORWARD is a function moving the cursor to the next end of block.
MOVE-BACKWARD is a function moving the cursor to the previous beginning of
block.
Both functions return t if succeeded, return nil otherwise."
  (save-excursion
    (let* ((beginning (and (funcall move-backward) (point)))
           (end (and beginning (funcall move-forward) (point))))
      (and end (cons beginning end)))))

(defun swift-mode:containing-generic-block-region (original-region
                                                   move-forward
                                                   move-backward
                                                   &optional
                                                   preferred-direction)
  "Return list representing a region of containing generic block.

Its first element is a cons representing the region.
The second element is a symbol one of `containing', `preceding', or `following',
which indicates which defun is marked.

The region contains ORIGINAL-REGION.

MOVE-FORWARD is a function moving the cursor to the next end of block.
MOVE-BACKWARD is a function moving the cursor to the previous beginning of
block.
Both functions return t if succeeded, return nil otherwise.
If PREFERRED-DIRECTION is `preceding' try to mark the preceding defun first.
Otherwise, try to mark the following one."
  (let* ((start-pos (min (car original-region) (cdr original-region)))
         (end-pos (max (car original-region) (cdr original-region)))
         region extended)
    (cond

     ;; /* original-region is here */ func foo() {
     ;;                               }
     ((progn
        (setq region
              (if (eq preferred-direction 'preceding)
                  (save-excursion
                    (goto-char start-pos)
                    (swift-mode:preceding-generic-block-region
                     move-forward move-backward))
                (save-excursion
                  (goto-char end-pos)
                  (swift-mode:following-generic-block-region
                   move-forward move-backward))))
        (setq extended (swift-mode:extend-region-with-spaces region))
        (and extended (<= (car extended) start-pos end-pos (cdr extended))))
      (list region preferred-direction))

     ;; func foo() {
     ;; } /* original-region is here */
     ((progn
        (setq region
              (if (eq preferred-direction 'preceding)
                  (save-excursion
                    (goto-char end-pos)
                    (swift-mode:following-generic-block-region
                     move-forward move-backward))
                (save-excursion
                  (goto-char start-pos)
                  (swift-mode:preceding-generic-block-region
                   move-forward move-backward))))
        (setq extended (swift-mode:extend-region-with-spaces region))
        (and extended (<= (car extended) start-pos end-pos (cdr extended))))
      (list region
            (if (eq preferred-direction 'preceding) 'following 'preceding)))

     ;; class Foo {
     ;;   func foo() {
     ;;   }
     ;;
     ;;   /* original-region is here */
     ;;
     ;;   func bar() {
     ;;   }
     ;; }
     (t
      (save-excursion
        (catch 'swift-mode:found-block
          (let ((start start-pos)
                (end end-pos))
            (goto-char end-pos)
            (while (and (funcall move-forward) (/= end (point)))
              (setq end (point))
              (save-excursion
                (funcall move-backward)
                (when (<= (point) start-pos end-pos end)
                  (throw 'swift-mode:found-block
                         (list (cons (point) end) 'containing)))))
            (when (= end (point))
              ;; Got unmatched parens.
              ;; Scans backward.
              (goto-char start-pos)
              (while (and (funcall move-backward) (/= start (point)))
                (setq start (point))
                (save-excursion
                  (funcall move-forward)
                  (when (<= start start-pos end-pos (point))
                    (throw 'swift-mode:found-block
                           (list (cons start (point)) 'containing)))
                  (funcall move-backward)
                  (when (/= start (point))
                    (throw 'swift-mode:found-block
                           (list (cons start end) 'containing)))))))
          (list (cons (point-min) (point-max)) 'containing)))))))

(defun swift-mode:extend-region-with-spaces (region)
  "Return REGION extended with surrounding spaces."
  (and region
       (let ((beginning (car region))
             (end (cdr region)))
         (save-excursion
           (goto-char beginning)
           (skip-syntax-backward " ")
           (setq beginning (point)))
         (save-excursion
           (goto-char end)
           (skip-syntax-forward " ")
           (setq end (point)))
         (cons beginning end))))

(defun swift-mode:narrow-to-generic-block
    (&optional include-comments move-forward move-backward)
  "Make text outside current generic block invisible.

If the point is between blocks, narrow depend on
`swift-mode:mark-defun-preference'.

Preceding comments are included if INCLUDE-COMMENTS is non-nil.
Interactively, the behavior depends on ‘narrow-to-defun-include-comments’.

MOVE-FORWARD is a function moving the cursor to the next end of block.
MOVE-BACKWARD is a function moving the cursor to the previous beginning of
block.
Both functions return t if succeeded, return nil otherwise."
  (let ((restriction (cons (point-min) (point-max)))
        region
        extended)
    (save-excursion
      (widen)
      (setq region
            (cond
             ((eq swift-mode:mark-defun-preference 'containing)
              (nth 0 (swift-mode:containing-generic-block-region
                      (cons (point) (point))
                      move-forward move-backward)))
             ((eq swift-mode:mark-defun-preference 'preceding)
              (swift-mode:preceding-generic-block-region
               move-forward move-backward))
             ((eq swift-mode:mark-defun-preference 'following)
              (swift-mode:following-generic-block-region
               move-forward move-backward))))
      (setq extended
            (and region (swift-mode:extend-region-with-spaces region)))
      (when (and extended include-comments)
        (save-excursion
          (goto-char (car extended))
          ;; Includes comments.
          (forward-comment (- (point)))
          ;; Excludes spaces and line breaks.
          (skip-syntax-forward " >")
          ;; Includes indentation.
          (skip-syntax-backward " ")
          (setcar extended (point))))
      (if extended
          (progn (narrow-to-region (car extended) (cdr extended)) extended)
        (narrow-to-region (car restriction) (cdr restriction))
        nil))))

(defun swift-mode:backward-sentence (&optional arg)
"Skip backward sentences or statements.

In comments or strings, skip a sentence.  Otherwise, skip a statement.

With ARG, repeat ARG times.  If ARG is negative, Skip forwards.

Return t if a sentence is found.  Return nil otherwise."
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (swift-mode:forward-sentence (- arg))
    (let ((result t))
      (while (and result (< 0 arg))
        (setq result (swift-mode:backward-sentence-1))
        (setq arg (1- arg)))
      result)))

(defun swift-mode:forward-sentence-1 ()
  "Skip forward a sentence or a statement.

In comments or strings, skip a sentence.  Otherwise, skip a statement."
  (let ((chunk (swift-mode:chunk-after)))
    (cond
     ((swift-mode:chunk:comment-p chunk)
      (swift-mode:forward-sentence-inside-comment
       (swift-mode:chunk:single-line-comment-p chunk)))
     ((swift-mode:chunk:string-p chunk)
      (swift-mode:forward-sentence-inside-string))
     ;; Spaces at the beginning of 2nd and following lines.
     ;;
     ;; class Foo {
     ;;   // aaa
     ;;
     ;;   // bbb
     ;;   // ccc ← spaces before //
     ;;   // ddd ← spaces before //
     ;;   func foo() { // eee
     ;;   }
     ;; }
     ;;
     ;; Not including spaces before the first line of blocks.
     ((save-excursion
        (skip-syntax-backward " ")
        (skip-chars-backward "/")
        (skip-syntax-backward " ")
        (and (bolp)
             (looking-at "[ \t]*//")
             (not (bobp))
             (progn
               (backward-char)
               (swift-mode:chunk:comment-p (swift-mode:chunk-after)))))
      (forward-line 0)
      (skip-syntax-forward " ")
      (forward-char 2)
      (swift-mode:forward-sentence-inside-comment t))
     (t
      (swift-mode:forward-sentence-inside-code)))))

(defun swift-mode:backward-sentence-1 ()
  "Skip backward a sentence or a statement.

In comments or strings, skip a sentence.  Otherwise, skip a statement."
  (let ((chunk (swift-mode:chunk-after)))
    (cond
     ((swift-mode:chunk:comment-p chunk)
      (swift-mode:backward-sentence-inside-comment
       (swift-mode:chunk:single-line-comment-p chunk)))
     ((swift-mode:chunk:string-p chunk)
      (swift-mode:backward-sentence-inside-string))
     ;; Spaces at the beginning of 2nd and following lines.
     ;;
     ;; class Foo {
     ;;   // aaa
     ;;
     ;;   // bbb
     ;;   // ccc ← spaces before //
     ;;   // ddd ← spaces before //
     ;;   func foo() { // eee
     ;;   }
     ;; }
     ;;
     ;; Not including spaces before the first line of blocks.
     ((save-excursion
        (skip-syntax-backward " ")
        (and (bolp)
             (looking-at "[ \t]*//")
             (not (bobp))
             (progn
               (backward-char)
               (nth 4 (syntax-ppss)))))
      (forward-line 0)
      (skip-syntax-forward " ")
      (forward-char 2)
      (swift-mode:backward-sentence-inside-comment t))
     (t
      (swift-mode:backward-sentence-inside-code)))))

(defmacro swift-mode:with-temp-comment-buffer (&rest body)
  "Eval BODY inside a temporary buffer keeping sentence related variables."
  (declare (indent 0) (debug t))
  (let ((current-sentence-end (make-symbol "current-sentence-end"))
        (current-paragraph-start (make-symbol "current-paragraph-start"))
        (current-paragraph-separate (make-symbol "current-paragraph-separate"))
        (current-paragraph-ignore-fill-prefix
         (make-symbol "current-paragraph-ignore-fill-prefix"))
        (current-fill-prefix (make-symbol "current-fill-prefix")))
    `(let ((,current-sentence-end (sentence-end))
           (,current-paragraph-start paragraph-start)
           (,current-paragraph-separate paragraph-separate)
           (,current-paragraph-ignore-fill-prefix paragraph-ignore-fill-prefix)
           (,current-fill-prefix fill-prefix))
       (with-temp-buffer
         (setq-local sentence-end ,current-sentence-end)
         (setq-local paragraph-start ,current-paragraph-start)
         (setq-local paragraph-separate ,current-paragraph-separate)
         (setq-local paragraph-ignore-fill-prefix
                     ,current-paragraph-ignore-fill-prefix)
         (setq-local fill-prefix ,current-fill-prefix)
         ,@body))))

(defun swift-mode:forward-sentence-inside-comment (is-single-line)
  "Skip forward a sentence in a comment.

IS-SINGLE-LINE should be non-nil when called inside a single-line comment."
  (when (and (not is-single-line)
             (eq (char-before) ?/)
             (eq (char-after) ?*))
    (forward-char))
  (when (and is-single-line
             (< (point) (save-excursion
                          (forward-line 0)
                          (if (looking-at "\\s *//+")
                              (match-end 0)
                            (point)))))
    (goto-char (match-end 0)))
  (let ((current-buffer (current-buffer))
        (pos (point))
        (comment-block-end-position
         (if is-single-line
             (swift-mode:comment-block-end-position-single-line)
           (swift-mode:comment-block-end-position-multiline)))
        offset-from-line-end
        line-count)
    (swift-mode:with-temp-comment-buffer
      (insert-buffer-substring current-buffer pos comment-block-end-position)
      (goto-char (point-min))
      ;; Removes comment starters.
      (save-excursion
        (if is-single-line
            (while (re-search-forward "^[ \t]*/+[ \t]*" nil t)
              (replace-match ""))
          (when (and (not (looking-at "\\*+/"))
                     (looking-at "\\*+"))
            (replace-match ""))))
      ;; Forwards sentence.
      (let ((old-position (point)))
        (unless (eobp)
          (forward-sentence))
        ;; Backwards spaces at end.
        (when (save-excursion
                (skip-syntax-forward " >")
                (eobp))
          (when (and (not is-single-line)
                     (eq (char-before) ?/))
            (backward-char)
            (skip-chars-backward "*"))
          (skip-syntax-backward " >")
          (when (< (point) old-position)
            (goto-char old-position)))
        ;; Locates current position.
        (setq offset-from-line-end (- (line-end-position) (point)))
        (setq line-count 0)
        (while (< old-position (line-beginning-position))
          (forward-line -1)
          (setq line-count (1+ line-count)))))
    (forward-line line-count)
    (goto-char (- (if (and (not is-single-line)
                           (eq (line-end-position)
                               (save-excursion
                                 (goto-char comment-block-end-position)
                                 (line-end-position))))
                      comment-block-end-position
                    (line-end-position))
                  offset-from-line-end))
    (or (/= (point) pos)
        (progn
          (goto-char comment-block-end-position)
          (swift-mode:forward-sentence-inside-code nil)))))

(defun swift-mode:backward-sentence-inside-comment (is-single-line)
  "Skip backward a sentence in a comment.

IS-SINGLE-LINE should be non-nil when called inside a single-line comment."
  (when (and (not is-single-line)
             (eq (char-before) ?*)
             (eq (char-after) ?/))
    (backward-char))
  (let ((current-buffer (current-buffer))
        (pos (point))
        (line-end-position (line-end-position))
        (comment-block-beginning-position
         (if is-single-line
             (swift-mode:comment-block-beginning-position-single-line)
           (swift-mode:comment-block-beginning-position-multiline)))
        offset-from-line-end
        line-count)
    (swift-mode:with-temp-comment-buffer
      (insert-buffer-substring
       current-buffer comment-block-beginning-position line-end-position)
      (goto-char (- (line-end-position) (- line-end-position pos)))
      ;; Removes comment starters.
      (save-excursion
        (goto-char (point-min))
        (if is-single-line
            (while (re-search-forward "^[ \t]*/+[ \t]*" nil t)
              (replace-match ""))
          (when (looking-at "[ \t]*/\\*+[ \t\n]*")
            (replace-match "")))
        ;; Removes empty lines at the beginning.
        (goto-char (point-min))
        (when (looking-at "\n+")
          (replace-match "")))
      ;; Backwards sentence.
      (let ((old-position (point)))
        (backward-sentence)
        ;; Locates current position.
        (setq offset-from-line-end (- (line-end-position) (point)))
        (setq line-count 0)
        (while (< (line-end-position) old-position)
          (forward-line 1)
          (setq line-count (1+ line-count)))))
    (forward-line (- line-count))
    (goto-char (- (line-end-position) offset-from-line-end))
    (or (< (point) pos)
        (progn
          (goto-char comment-block-beginning-position)
          (swift-mode:backward-sentence-inside-code t)))))

(defun swift-mode:comment-block-end-position-single-line ()
  "Return the position of the end of a single-line comment block.

A single-line comment block consists of a single-line comments at the beginning
of lines.  Empty lines split blocks.  Example:
    // A block begins here.
    //
    // ...
    //
    // The block ends here.

    // Another block begins here.
    //
    // ...
    //
    // The block ends here.
    foo() // This comment is not a part of the block."
  (save-excursion
    (let ((comment-block-end-position nil))
      (while (and (swift-mode:chunk:single-line-comment-p
                   (swift-mode:chunk-after))
                  (not (eobp)))
        (end-of-line)
        (setq comment-block-end-position (point))
        (forward-line)
        (skip-chars-forward " \t")
        (skip-chars-forward "/"))
      comment-block-end-position)))

(defun swift-mode:comment-block-beginning-position-single-line ()
  "Return the position of the beginning of a single-line comment block.

A single-line comment block consists of a single-line comments at the beginning
of lines.  Empty lines split blocks.  Example:
    // A block begins here.
    //
    // ...
    //
    // The block ends here.

    // Another block begins here.
    //
    // ...
    //
    // The block ends here.
    foo() // This comment is not a part of the block."
  (save-excursion
    (let (chunk
          (comment-block-beginning-position nil))
      (while (progn
               (setq chunk (swift-mode:chunk-after))
               (swift-mode:chunk:single-line-comment-p chunk))
        (goto-char (swift-mode:chunk:start chunk))
        (setq comment-block-beginning-position (point))
        (skip-syntax-backward " ")
        (unless (bobp) (backward-char)))
      comment-block-beginning-position)))

(defun swift-mode:comment-block-end-position-multiline ()
  "Return the position of the end of a multiline comment."
  (save-excursion
    (goto-char (swift-mode:chunk:start (swift-mode:chunk-after)))
    (forward-comment 1)
    (point)))

(defun swift-mode:comment-block-beginning-position-multiline ()
  "Return the position of the beginning of a multiline comment."
  (swift-mode:chunk:start (swift-mode:chunk-after)))

(defun swift-mode:forward-sentence-inside-string ()
  "Skip forward a sentence in a string."
  (let ((string-end-position
         (save-excursion
           (goto-char (swift-mode:chunk:start (swift-mode:chunk-after)))
           (swift-mode:forward-string-chunk)
           (point))))
    (forward-sentence)
    (or (<= (point) string-end-position)
        (progn
          (goto-char string-end-position)
          (if (eq (char-before) ?\()
              (swift-mode:forward-sentence-inside-interpolated-expression)
            (swift-mode:forward-sentence-inside-code t))))))

(defun swift-mode:backward-sentence-inside-string ()
  "Skip backward a sentence in a string."
  ;; Skips quotes at the end.
  (when (and
         (eq (char-after) ?\")
         (save-excursion
           (skip-chars-forward "\"")
           (equal (get-text-property (1- (point)) 'syntax-table)
                  (string-to-syntax "|"))))
    (skip-chars-backward "\""))
  (let ((pos (point))
        (string-beginning-position
         (swift-mode:chunk:start (swift-mode:chunk-after))))
    (backward-sentence)
    (or (<= string-beginning-position (point))
        (progn
          (goto-char string-beginning-position)
          (cond
           ((and (looking-at "\"\"\"")
                 (save-excursion
                   (skip-chars-forward "\"")
                   (skip-syntax-forward " >")
                   (< (point) pos)))
            (skip-chars-forward "\"")
            (skip-syntax-forward " >")
            t)
           ((eq (char-after) ?\))
            (swift-mode:backward-sentence-inside-interpolated-expression))
           (t
            (swift-mode:backward-sentence-inside-code t)))))))

(defun swift-mode:forward-sentence-inside-interpolated-expression ()
  "Skip forward a sentence in a interpolated expression."
  (let* ((string-chunk (swift-mode:find-following-string-chunk))
         (interpolated-expression-end-position
          (swift-mode:token:start string-chunk)))
    (swift-mode:forward-statement)
    (or (<= (point) interpolated-expression-end-position)
        (progn
          (goto-char interpolated-expression-end-position)
          (forward-char)
          (swift-mode:forward-sentence-inside-string)))))

(defun swift-mode:backward-sentence-inside-interpolated-expression ()
  "Skip backward a sentence in a interpolated expression."
  (let* ((string-chunk (swift-mode:find-preceeding-string-chunk))
         (interpolated-expression-beginning-position
          (swift-mode:token:end string-chunk)))
    (swift-mode:backward-statement)
    (or (<= interpolated-expression-beginning-position (point))
        (progn
          (goto-char interpolated-expression-beginning-position)
          (backward-char)
          (swift-mode:backward-sentence-inside-string)))))

(defun swift-mode:find-following-string-chunk ()
  "Return the following string-chunk token."
  (save-excursion
    (let (token)
      (while (progn
               (setq token (swift-mode:forward-token-or-list))
               (not (memq
                     (swift-mode:token:type token)
                     '(outside-of-buffer
                       string-chunk-after-interpolated-expression)))))
      token)))

(defun swift-mode:find-preceeding-string-chunk ()
  "Return the preceeding string-chunk token."
  (save-excursion
    (swift-mode:backward-sexps-until
     '(string-chunk-before-interpolated-expression))))

(defun swift-mode:forward-sentence-inside-code
    (&optional keep-position-if-at-end-of-sentence)
  "Skip forward a statement.

If KEEP-POSITION-IF-AT-END-OF-SENTENCE is non-nil and the cursor is already at
the end of a sentence, keep the position."
  (if (and (get-text-property (point) 'syntax-multiline)
           (not (bobp))
           ;; Not just before a string.
           (get-text-property (1- (point)) 'syntax-multiline))
      (swift-mode:forward-sentence-inside-interpolated-expression)
    (if keep-position-if-at-end-of-sentence
        (progn (swift-mode:end-of-statement) t)
      (let ((pos (point)))
        (swift-mode:forward-statement)
        (< pos (point))))))

(defun swift-mode:backward-sentence-inside-code
    (&optional keep-position-if-at-beginning-of-sentence)
  "Skip backward a statement.

If KEEP-POSITION-IF-AT-BEGINNING-OF-SENTENCE is non-nil and the cursor is
already at the beginning of a sentence, keep the position."
  (if (and (get-text-property (point) 'syntax-multiline)
           (not (bobp))
           ;; Not just before a string.
           (get-text-property (1- (point)) 'syntax-multiline))
      (swift-mode:backward-sentence-inside-interpolated-expression)
    (if keep-position-if-at-beginning-of-sentence
        (progn (swift-mode:beginning-of-statement) t)
      (let ((pos (point)))
        (swift-mode:backward-statement)
        (< (point) pos)))))

(defun swift-mode:kill-sentence (&optional arg)
  "Kill from the point to the end of sentences.

With ARG, kill to the end of the ARG-th sentence.  If ARG is negative, kill
backwards."
  (interactive "p")
  (kill-region
   (point)
   (save-excursion (swift-mode:forward-sentence arg) (point))))

(defun swift-mode:backward-kill-sentence (&optional arg)
  "Kill from the point to the beginning of sentences.

With ARG, kill to the beginning of the ARG-th sentence.  If ARG is negative,
kill forwards."
  (interactive "p")
  (kill-region
   (point)
   (save-excursion (swift-mode:backward-sentence arg) (point))))

(defun swift-mode:mark-sentence (&optional arg allow-extend)
  "Put mark at the end of sentence, point at the beginning of sentence.

If the point is between sentence, mark depend on
`swift-mode:mark-defun-preference'.

If ARG is a positive number, mark that many following sentences.  If ARG is
negative, reverse direction of marking.  If those sentences have lesser
nesting level than the initial one, mark the whole outer sentence.

If ALLOW-EXTEND is non-nil or called interactively, and the command is repeated
or the region is active, mark the following (if the point is before the mark)
or preceding (if the point is after the mark) sentence."
  (interactive (list (prefix-numeric-value current-prefix-arg) t))
  (let ((region (swift-mode:mark-generic-block arg
                                               allow-extend
                                               #'swift-mode:forward-sentence
                                               #'swift-mode:backward-sentence)))
    (if (and (not region)  (called-interactively-p 'interactive))
        (progn (message "No sentence found") nil)
      region)))

(defun swift-mode:narrow-to-sentence (&optional include-comments)
"Make text outside current sentence invisible.

If the point is between sentences, narrow depend on
`swift-mode:mark-defun-preference'.

Preceding comments are included if INCLUDE-COMMENTS is non-nil.
Interactively, the behavior depends on ‘narrow-to-defun-include-comments’."
(interactive (list (and (boundp 'narrow-to-defun-include-comments)
                          narrow-to-defun-include-comments)))
  (let ((region (swift-mode:narrow-to-generic-block
                 include-comments
                 #'swift-mode:forward-sentence
                 #'swift-mode:backward-sentence)))
    (if (and  (not region) (called-interactively-p 'interactive))
        (progn (message "No sentence found") nil)
      region)))

(defun swift-mode:current-defun-name ()
  "Return fully qualified name of defun under the point."
  (save-excursion
    (let ((token-list (reverse (swift-mode:current-defun-name-token-list))))
      (if token-list
          (mapconcat #'swift-mode:token:text token-list ".")
        nil))))

(defun swift-mode:current-defun-name-token-list ()
  "Return a list of defun name tokens under the point.

The first element is the name token of the current defun.  The rest are the ones
of ancestors."
  (if (bobp)
      nil
    (let ((name-token (swift-mode:current-defun-name-token)))
      (swift-mode:backward-sexps-until '({))
      (if name-token
          (cons name-token (swift-mode:current-defun-name-token-list))
        (swift-mode:current-defun-name-token-list)))))

(defun swift-mode:current-defun-name-token ()
  "Return the name token of the defun under the point."
  (let ((pos (point))
        keyword-token
        keyword-text
        next-token
        name-token)
    (goto-char (caar (swift-mode:containing-generic-block-region
                      (cons (point) (point))
                      #'swift-mode:end-of-defun
                      #'swift-mode:beginning-of-defun)))

    (save-excursion
      (setq keyword-token (swift-mode:find-defun-keyword))
      (setq keyword-text (swift-mode:token:text keyword-token))
      (when keyword-token
        (goto-char (swift-mode:token:end keyword-token)))
      (setq
       name-token
       (cond
        ((member keyword-text
                 '("typealias" "associatedtype" "precedencegroup" "func"
                   "class" "enum" "struct" "protocol" "extension"))
         (swift-mode:forward-token))

        ((member keyword-text '("init" "deinit" "subscript"))
         keyword-token)

        ((member keyword-text '("case" "var" "let"))
         ;; enum Foo {
         ;;   case A, B(x: (Int, Int)), C
         ;; }
         ;;
         ;; class Foo {
         ;;   let x = 1,
         ;;       y = 1,
         ;;       z = 1
         ;;   var x {
         ;;     get {
         ;;       return 1
         ;;     }
         ;;   }
         ;;   var x = 1 {
         ;;     willSet {
         ;;     }
         ;;   }
         ;;
         ;;   let (x, y) = (1, 1) // not supported yet
         ;; }
         (while (< (point) pos)
           (setq next-token (swift-mode:forward-token-or-list)))
         (when next-token
           (goto-char (swift-mode:token:start next-token)))
         (goto-char (swift-mode:token:end
                     (swift-mode:backward-sexps-until (list keyword-text '\,))))
         (setq next-token (swift-mode:forward-token))
         (if (and
              (eq (swift-mode:token:type next-token) 'identifier)
              (not
               (equal (swift-mode:token:text (swift-mode:forward-token)) ".")))
             next-token
           ;; FIXME: Complex patterns.
           nil))

        ((member keyword-text '("prefix" "postfix" "infix"))
         (and (equal (swift-mode:token:text (swift-mode:forward-token))
                     "operator")
              (swift-mode:forward-token)))

        ;; Ignored: "import" "get" "set" "willSet" "didSet"
        (t nil))))
    (if (eq (swift-mode:token:type name-token) 'identifier)
        name-token
      nil)))

(provide 'swift-mode-beginning-of-defun)

;;; swift-mode-beginning-of-defun.el ends here
