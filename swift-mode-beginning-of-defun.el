;;; swift-mode-beginning-of-defun.el --- Major-mode for Apple's Swift programming language, beginning/end-of-defun. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 taku0

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
;; The end of a defun is just after the close curly brace.
;;
;; The beginning of a defun is the beginning of:
;; - "func" keyword,
;; - its modifiers or attributes,
;; - comments on the same line.
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
        (progn
          ;; `swift-mode:beginning-of-defun-1' assumes the point is after
          ;; the open curly brace of defun. So moving the point to just after
          ;; the open curly brace if the current statement has one.
          (while (not
                  (memq
                   (swift-mode:token:type (swift-mode:forward-token))
                   '({ implicit-\; \; } outside-of-buffer))))
          (backward-char)
          ;; Skips implicit-;
          (forward-comment (point-max))
          (when (eq (char-after) ?{)
            (forward-char))

          (setq result (swift-mode:beginning-of-defun-1
                        #'swift-mode:backward-token-or-list))
          (when (and result (< (point) pos))
            (setq arg (1- arg)))

          (while (and result (< 0 arg))
            (setq result (swift-mode:beginning-of-defun-1
                          #'swift-mode:backward-token-or-list))
            (setq arg (1- arg))))
      ;; Moving forward
      (setq result (swift-mode:beginning-of-defun-1
                    (lambda ()
                      (prog1 (swift-mode:forward-token-or-list)
                        (forward-comment (point-max))))))
      (when (and result (< pos (point)))
        (setq arg (1+ arg)))
      (while (and result (< arg 0))
        ;; Skips the current statement
        (while (not
                (memq
                 (swift-mode:token:type (swift-mode:forward-token-or-list))
                 '({} implicit-\; \; } outside-of-buffer))))
        (setq result (swift-mode:beginning-of-defun-1
                      (lambda ()
                        (prog1 (swift-mode:forward-token-or-list)
                          (forward-comment (point-max))))))
        (setq arg (1+ arg))))
    (and result
         (eq this-command 'swift-mode:beginning-of-defun)
         (not (eq last-command 'swift-mode:beginning-of-defun))
         (not (region-active-p))
         (push-mark pos))
    result))

(defun swift-mode:beginning-of-defun-1 (next-token-function)
  "Goto the beginning of a defun.

NEXT-TOKEN-FUNCTION skips the preceding/following token."
  (catch 'swift-mode:found-defun
    (while (not (eq (swift-mode:token:type (funcall next-token-function))
                    'outside-of-buffer))
      (when (save-excursion (swift-mode:is-point-before-body-of-defun))
        (swift-mode:beginning-of-statement)
        (throw 'swift-mode:found-defun t)))
    nil))

(defun swift-mode:is-point-before-body-of-defun ()
  "Return t it the point is just before the body of a defun.

Return nil otherwise."
  (and
   (eq (char-after) ?{)
   (progn
     ;; Skips implicit ;
     (forward-comment (- (point)))
     (let* ((defun-keywords '("class" "struct" "protocol" "enum" "extension"
                              "func" "operator" "var" "get" "set" "willSet"
                              "didSet" "deinit" "subscript"))
            (previous-token (swift-mode:backward-token-or-list))
            (previous-type (swift-mode:token:type previous-token))
            (previous-text (swift-mode:token:text previous-token)))
       (while (and
               (not (eq previous-type 'outside-of-buffer))
               (not (memq previous-type swift-mode:statement-parent-tokens))
               (not (member previous-text swift-mode:statement-parent-tokens))
               (not (member previous-text defun-keywords))
               (not (and (equal previous-text "init")
                         (save-excursion
                           ;; Excludes self.init() {}
                           (not
                            (equal
                             (swift-mode:token:text (swift-mode:backward-token))
                             "."))))))
         (setq previous-token (swift-mode:backward-token-or-list))
         (setq previous-type (swift-mode:token:type previous-token))
         (setq previous-text (swift-mode:token:text previous-token)))
       (or (equal previous-text "init")
           (member previous-text defun-keywords))))))

(defun swift-mode:beginning-of-statement ()
  "Move backward to the beginning of a statement or some kind of expression.

Intended for internal use."
  (let ((parent (swift-mode:backward-sexps-until
                 swift-mode:statement-parent-tokens)))
    (if (and
         (eq (swift-mode:token:type parent) 'implicit-\;)
         (save-excursion
           (goto-char (swift-mode:token:end parent))
           (eq
            (swift-mode:token:type (swift-mode:forward-token))
            '{)))
        (progn
          (forward-comment (- (point)))
          (swift-mode:beginning-of-statement))
      ;; Excludes comments on previous lines.
      (goto-char (swift-mode:token:end parent))
      (forward-comment (- (point)))
      (setq parent (save-excursion (swift-mode:backward-token)))
      (forward-comment (point-max))
      (swift-mode:goto-non-comment-bol)
      (when (< (point) (swift-mode:token:end parent))
        (goto-char (swift-mode:token:end parent)))
      (swift-mode:skip-whitespaces))))

(defun swift-mode:end-of-defun (&optional arg)
  "Move forward to the end of a defun.

See `end-of-defun' for ARG.

Return t if a defun is found.  Return nil otherwise.

Push mark at previous position if this is called as a command, not repeatedly,
and the region is not active."
  (interactive "p")
  (setq arg (or arg 1))
  (let (result
        (pos (point)))
    (if (<= 0 arg)
        (while (< 0 arg)
          (setq result (swift-mode:end-of-defun-1
                        #'swift-mode:forward-token-or-list))
          (setq arg (1- arg)))
      (while (< arg 0)
        (setq result (swift-mode:end-of-defun-1
                      (lambda ()
                        (prog1 (swift-mode:backward-token-or-list)
                          (forward-comment (- (point)))))))
        (setq arg (1+ arg))))
    (and result
         (eq this-command 'swift-mode:end-of-defun)
         (not (eq last-command 'swift-mode:end-of-defun))
         (not (region-active-p))
         (push-mark pos))
    result))

(defun swift-mode:end-of-defun-1 (next-token-function)
  "Goto the end of a defun.

NEXT-TOKEN-FUNCTION skips the preceding/following token."
  (catch 'swift-mode:found-defun
    (while (not (eq (swift-mode:token:type (funcall next-token-function))
                    'outside-of-buffer))
      (when (and (eq (char-before) ?})
                 (save-excursion
                   (backward-list)
                   (swift-mode:is-point-before-body-of-defun)))
        (throw 'swift-mode:found-defun t)))
    nil))

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
        ;; Marks the whole outer defun if it has lesser nesting level.
        (if forward-p
            (goto-char (min (point)
                            (save-excursion
                              (goto-char (mark))
                              (swift-mode:beginning-of-defun)
                              (point))))
          (goto-char (max (point)
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

If the point is between defuns, mark depend on
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
