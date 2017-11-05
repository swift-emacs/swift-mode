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
      (let ((forward-p (<= (point) (mark))))
        (set-mark
         (save-excursion
           (goto-char (mark))
           (if forward-p
               (swift-mode:end-of-defun)
             (swift-mode:beginning-of-defun))
           (point)))
        ;; Marks the whole outer defun if the mark got out of the outer defun.
        (goto-char
         (if forward-p
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

(provide 'swift-mode-beginning-of-defun)

;;; swift-mode-beginning-of-defun.el ends here
