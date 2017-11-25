;;; swift-mode-beginning-of-defun.el --- Major-mode for Apple's Swift programming language, beginning/end-of-defun. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 taku0

;; Authors: taku0 (http://github.com/taku0)
;;
;; Version: 3.0.0
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

;;;###autoload
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
            "enum" "struct" "class" "protocol" "extension"
            "func" "init" "deinit" "subscript" "get" "set" "willSet" "didSet"
            "prefix" "postfix" "infix" "precedencegroup"
            "var" "let"
            "case"))
        (stop-tokens '(\; implicit-\; {} } \) \]
                       anonymous-function-parameter-in outside-of-buffer)))
    (while (not (or
                 (memq (swift-mode:token:type token) stop-tokens)
                 (member (swift-mode:token:text token) defun-keywords)))
      (setq token (swift-mode:forward-token-or-list)))
    (when (member (swift-mode:token:text token) defun-keywords)
      token)))

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
    (setq parent (save-excursion (swift-mode:backward-token)))
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
    (when (eq pos (point))
      (swift-mode:backward-token-or-list)
      (swift-mode:beginning-of-statement))))

(defun swift-mode:end-of-statement ()
  "Move forward to the end of a statement.

When called at the end of a statement, keep the position.

Return the next token.
Intended for internal use."
  (let ((pos (point))
        (previous-token (save-excursion (swift-mode:backward-token)))
        next-token)
    (cond
     ((and
       (memq (swift-mode:token:type previous-token)
             '(\; anonymous-function-parameter-in))
       (eq (swift-mode:token:end previous-token) pos))
      ;; Already at the eod of statement.  Returns next token.
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
    (while (eq (swift-mode:token:type
                (save-excursion (swift-mode:forward-token)))
               '\;)
      (setq token (swift-mode:forward-token)))
    (if (memq (swift-mode:token:type token)
              '(\; anonymous-function-parameter-in))
        (goto-char (swift-mode:token:end token))
      (goto-char (swift-mode:token:start token)))
    (cond
     ((eq (swift-mode:token:type token) 'outside-of-buffer)
      (forward-comment (- (point)))
      (when (< (point) pos)
        (goto-char (swift-mode:token:end token)))
      token)
     ((eq (swift-mode:token:type token) '})
      (forward-comment (- (point)))
      (if (< (point) pos)
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
        (pos (point)))
    (if (<= 0 arg)
        ;; Moving forward
        (while (and result (< 0 arg))
          (swift-mode:forward-statement)
          (when (save-excursion
                  (swift-mode:beginning-of-statement)
                  (swift-mode:find-defun-keyword))
            (setq arg (1- arg)))
          (when (and (< 0 arg)
                      (eq (swift-mode:token:type (swift-mode:forward-token))
                          'outside-of-buffer))
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
                (token (swift-mode:backward-token)))
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


(defun swift-mode:mark-defun (&optional allow-extend)
  "Put mark at the end of defun, point at the beginning of defun.

If the point is between defuns, mark depend on
`swift-mode:mark-defun-preference'.

If ALLOW-EXTEND is non-nil or called interactively, and the command is repeated
or the region is active, mark the following (if the point is before the mark)
or preceding (if the point is after the mark) defun.  If that defun has lesser
nesting level, mark the whole outer defun."
  (interactive (list t))
  (if (and allow-extend
           (or
            (and (eq last-command this-command) (mark t))
            (region-active-p)))
      ;; Extends region.
      (let ((is-forward (<= (point) (mark))))
        (set-mark
         (save-excursion
           (goto-char (mark))
           (if is-forward
               (swift-mode:end-of-defun)
             (swift-mode:beginning-of-defun))
           (point)))
        ;; Marks the whole outer defun if the mark got out of the outer defun.
        (goto-char
         (if is-forward
             (min (point)
                  (save-excursion
                    (goto-char (mark))
                    (swift-mode:beginning-of-defun)
                    (point)))
           (max (point)
                (save-excursion
                  (goto-char (mark))
                  (swift-mode:end-of-defun)
                  (point))))))
    ;; Marks new region.
    (let ((region
           (cond
            ((eq swift-mode:mark-defun-preference 'containing)
             (swift-mode:containing-defun-region))
            ((eq swift-mode:mark-defun-preference 'preceding)
             (swift-mode:preceding-defun-region))
            ((eq swift-mode:mark-defun-preference 'following)
             (swift-mode:following-defun-region)))))
      (if region
          (progn (push-mark (cdr region) nil t)
                 (goto-char (car region))
                 region)
        (when (called-interactively-p 'interactive)
          (message "No defun found"))
        nil))))

(defun swift-mode:following-defun-region ()
  "Return cons representing a region of following defun."
  (save-excursion
    (let* ((end (and (swift-mode:end-of-defun) (point)))
           (beginning (and end (swift-mode:beginning-of-defun) (point))))
      (and beginning (cons beginning end)))))

(defun swift-mode:preceding-defun-region ()
  "Return cons representing a region of preceding defun."
  (save-excursion
    (let* ((beginning (and (swift-mode:beginning-of-defun) (point)))
           (end (and beginning (swift-mode:end-of-defun) (point))))
      (and end (cons beginning end)))))

(defun swift-mode:containing-defun-region ()
  "Return cons representing a region of containing defun."
  (let* ((pos (point))
         (region (swift-mode:following-defun-region))
         (extended (and region
                        (swift-mode:extend-defun-region-with-spaces region))))
    (cond
     ((and extended (<= (car extended) pos (cdr extended)))
      region)

     ((progn
        (setq region (swift-mode:preceding-defun-region))
        (setq extended (swift-mode:extend-defun-region-with-spaces region))
        (and extended (<= (car extended) pos (cdr extended))))
      region)

     (t
      (catch 'swift-mode:found-defun
        (while (swift-mode:end-of-defun)
          (let ((end (point)))
            (save-excursion
              (swift-mode:beginning-of-defun)
              (when (<= (point) pos end)
                (throw 'swift-mode:found-defun (cons (point) end))))))
        (cons (point-min) (point-max)))))))

(defun swift-mode:extend-defun-region-with-spaces (region)
  "Return REGION extended with surrounding spaces."
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
    (cons beginning end)))

(defun swift-mode:narrow-to-defun (&optional include-comments)
  "Make text outside current defun invisible.

If the point is between defuns, narrow depend on
`swift-mode:mark-defun-preference'.

Preceding comments are included if INCLUDE-COMMENTS is non-nil.
Interactively, the behavior depends on ‘narrow-to-defun-include-comments’."
  (interactive (list narrow-to-defun-include-comments))
  (let ((restriction (cons (point-min) (point-max)))
        region
        extended)
    (save-excursion
      (widen)
      (setq region
            (cond
             ((eq swift-mode:mark-defun-preference 'containing)
              (swift-mode:containing-defun-region))
             ((eq swift-mode:mark-defun-preference 'preceding)
              (swift-mode:preceding-defun-region))
             ((eq swift-mode:mark-defun-preference 'following)
              (swift-mode:following-defun-region))))
      (setq extended
            (and region (swift-mode:extend-defun-region-with-spaces region)))
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
          (narrow-to-region (car extended) (cdr extended))
        (when (called-interactively-p 'interactive)
          (message "No defun found"))
        (narrow-to-region (car restriction) (cdr restriction))))))

(defun swift-mode:forward-sentence (&optional arg)
  "Skip forward sentences or statements.

In comments or strings, skip a sentence.  Otherwise, skip a stateement.

With ARG, repeat ARG times.  If ARG is negative, Skip backwards."
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (swift-mode:backward-sentence (- arg))
    (while (< 0 arg)
      (unless (swift-mode:forward-sentence-1)
        (setq arg 1))
      (setq arg (1- arg)))))

(defun swift-mode:backward-sentence (&optional arg)
"Skip backward sentences or statements.

In comments or strings, skip a sentence.  Otherwise, skip a stateement.

With ARG, repeat ARG times.  If ARG is negative, Skip forwards."
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (swift-mode:forward-sentence (- arg))
    (while (< 0 arg)
      (unless (swift-mode:backward-sentence-1)
        (setq arg 1))
      (setq arg (1- arg)))))

(defun swift-mode:forward-sentence-1 ()
  "Skip forward a sentence or a statement.

In comments or strings, skip a sentence.  Otherwise, skip a stateement."
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

In comments or strings, skip a sentence.  Otherwise, skip a stateement."
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

(defun swift-mode:forward-sentence-inside-comment (is-single-line)
  "Skip forward a sentence in a comment.

IS-SINGLE-LINE should be non-nil when called inside a single-line comment."
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
          (when (looking-at "\\*+")
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
    (goto-char (- (line-end-position) offset-from-line-end))
    (when (= (point) pos)
      (goto-char comment-block-end-position)
      (swift-mode:forward-sentence-inside-code))))

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
    (when (<= pos (point))
      (goto-char comment-block-beginning-position)
      (swift-mode:backward-sentence-inside-code))))

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
    (when (< string-end-position (point))
      (goto-char string-end-position)
      (if (eq (char-before) ?\()
          (swift-mode:forward-sentence-inside-interpolated-expression)
        (swift-mode:forward-sentence-inside-code t)))))

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
    (when (< (point) string-beginning-position)
      (goto-char string-beginning-position)
      (cond
       ((and (looking-at "\"\"\"")
             (save-excursion
               (skip-chars-forward "\"")
               (skip-syntax-forward " >")
               (< (point) pos)))
        (skip-chars-forward "\"")
        (skip-syntax-forward " >"))
       ((eq (char-after) ?\))
        (swift-mode:backward-sentence-inside-interpolated-expression))
       (t
        (swift-mode:backward-sentence-inside-code t))))))

(defun swift-mode:forward-sentence-inside-interpolated-expression ()
  "Skip forward a sentence in a interpolated expression."
  (let* ((string-chunk (swift-mode:find-following-string-chunk))
         (interpolated-expression-end-position
          (swift-mode:token:start string-chunk)))
    (swift-mode:forward-statement)
    (when (< interpolated-expression-end-position (point))
      (goto-char interpolated-expression-end-position)
      (forward-char)
      (swift-mode:forward-sentence-inside-string))))

(defun swift-mode:backward-sentence-inside-interpolated-expression ()
  "Skip backward a sentence in a interpolated expression."
  (let* ((string-chunk (swift-mode:find-preceeding-string-chunk))
         (interpolated-expression-beginning-position
          (swift-mode:token:end string-chunk)))
    (swift-mode:backward-statement)
    (when (< (point) interpolated-expression-beginning-position)
      (goto-char interpolated-expression-beginning-position)
      (backward-char)
      (swift-mode:backward-sentence-inside-string))))

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
    (&optional keep-position-if-at-end-of-statement)
  "Skip forward a statement.

If KEEP-POSITION-IF-AT-END-OF-STATEMENT is non-nil and the cursor is already at
the end of a statement, keep the position."
  (if (and (get-text-property (point) 'syntax-multiline)
           (not (bobp))
           ;; Not just before a string.
           (get-text-property (1- (point)) 'syntax-multiline))
      (swift-mode:forward-sentence-inside-interpolated-expression)
    (if keep-position-if-at-end-of-statement
        (swift-mode:end-of-statement)
      (swift-mode:forward-statement))))

(defun swift-mode:backward-sentence-inside-code
    (&optional keep-position-if-at-beginning-of-statement)
  "Skip backward a statement.

If KEEP-POSITION-IF-AT-BEGINNING-OF-STATEMENT is non-nil and the cursor is
already at the beginning of a statement, keep the position."
  (if (and (get-text-property (point) 'syntax-multiline)
           (not (bobp))
           ;; Not just before a string.
           (get-text-property (1- (point)) 'syntax-multiline))
      (swift-mode:backward-sentence-inside-interpolated-expression)
    (if keep-position-if-at-beginning-of-statement
        (swift-mode:beginning-of-statement)
      (swift-mode:backward-statement))))

(provide 'swift-mode-beginning-of-defun)

;;; swift-mode-beginning-of-defun.el ends here
