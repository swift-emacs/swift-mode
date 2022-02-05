;;; swift-mode-fill.el --- Major-mode for Apple's Swift programming language, paragraph filling. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Josh Caswell

;; Authors: Josh Caswell (https://github.com/woolsweater)

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

;; Routines for paragraph filling

;;; Code:

(require 'rx)
(require 'swift-mode-lexer)

(defcustom swift-mode:comment-fill-column 80
  "Fill column for comment wrapping in Swift code.

This may be different than the fill column for the rest of the source.  See
also `swift-mode:comment-fill-function'."
  :type 'integer
  :group 'swift
  :safe 'integerp)

(make-variable-buffer-local 'swift-mode:comment-fill-column)

(defconst swift-mode:doc-comment-annotation-re
  (let ((bullet '(any ?- ?+ ?*))
        (any-spacing '(* blank))
        (identifier '(+ (syntax word))))
    (rx-to-string
     `(seq
       ;; Explicitly include indentation here, for `forward-paragraph'
       ;; search (see usage of 'parstart' there)
       ,any-spacing
       ,bullet ,any-spacing ,identifier
       ;; For '- parameter foo:'
       (? ,any-spacing ,identifier)
       ,any-spacing ?:)))
  "Regex to match Swift documentation comment markup labels, like '- remark:'.

This is used by `swift-mode:comment-fill-function' to extend
`paragraph-start' such that the built-in fill functions recognize
these elements as the beginnings of their own paragraphs.")

(defsubst swift-mode:-find-slash-comment-edges (slashes)
  "Helper for `swift-mode:comment-fill-function' handling '///' style.

Search backwards and forwards for contiguous lines that
open (after any indentation) with SLASHES.  Return the buffer
locations for the beginning and end of the comment contents.  For
this style of comment, the content beginning is after the first
delimiter; the end is the end of the last contiguous line found.

Point may be anywhere in the comment when this is called."
  (let ((orig-point (point))
        start end)
    (back-to-indentation)
    (while (and (not (bobp))
                (looking-at-p slashes))
      (forward-line -1)
      (back-to-indentation))
    (setq start (progn
                  (search-forward slashes nil t)
                  (point)))
    (goto-char orig-point)
    (while (and (not (eobp))
                (looking-at-p slashes))
      (forward-line 1)
      (unless (eobp)
        (back-to-indentation)))
    (setq end (if (progn
                    (back-to-indentation)
                    (looking-at-p slashes))
                  ;; Edge case: comment is last thing in buffer with no trailing
                  ;; newline.
                  (point-max)
                (forward-line -1)
                (move-end-of-line 1)
                (point)))
    (cons start end)))

(defsubst swift-mode:-fix-up-star-comment-edges ()
  "Helper for `swift-mode:comment-fill-function' handling '/**' style.

Ensure each delimiter is on its own line, then return the buffer
locations for the beginning and end of the comment
contents (excluding the delimiters).

Point is assumed to be just after the opening delimiter and its
trailing whitespace (if any) when this is called."
  (when (not (looking-at-p "\n"))
    (delete-horizontal-space)
    (insert-and-inherit "\n")
    (indent-according-to-mode))

  (let ((start (point))
        (end (progn (re-search-forward "\\*+/")
                    (match-beginning 0))))
    (goto-char end)
    (skip-chars-backward " \t")
    (if (bolp)
        (setq end (- (point) 1))
      (insert-and-inherit "\n")
      (indent-according-to-mode))
    (cons start end)))

(defun swift-mode:comment-fill-function (justify)
  "Handle comment filling in Swift code.

Delegates to `fill-region' with `fill-column' bound to the value of
`swift-mode:comment-fill-column' so that comments can be wrapped at
different width than the rest of the source source.  JUSTIFY is as the
argument of the same name in `fill-region'.

The function determines which style of comment is at or around
point and does preliminary cleanup as needed (the built-in fill
functions do not handle the '/**' style of comment particularly
well)."
  ;; TODO A leading star on an empty line screws up paragraph calculation.
  ;; TODO Recognize fenced code blocks.
  ;; TODO Handle trailing comments.
  (let ((chunk (swift-mode:chunk-after)))
    (if (not (or (swift-mode:chunk:comment-p chunk)
                 (looking-at-p comment-start-skip)))
        ;; If not in a comment, just let `fill-paragraph' try to handle it
        nil
      (save-match-data
        (save-excursion
          ;; Move to opening delimiter if not already there.
          (let ((start (swift-mode:chunk:start chunk)))
            (when start
              (goto-char start)))
          (skip-syntax-forward " ")

          ;; We want these two bound to their special values when delegating to
          ;; `fill-region'.
          (let ((fill-column swift-mode:comment-fill-column)
                (paragraph-start (concat
                                  swift-mode:doc-comment-annotation-re
                                  "\\|"
                                  paragraph-start)))
            (cond

             ;; Slash-style comment
             ((looking-at "/\\{2,\\}")
              (let* ((slashes (match-string-no-properties 0))
                     (edges (swift-mode:-find-slash-comment-edges slashes))
                     (start (car edges))
                     (end (cdr edges)))
                ;; Factor the comment markers into paragraph recognition
                (let ((paragraph-start (concat "[[:blank:]]*" slashes
                                               "\\(?:" paragraph-start "\\)"))
                      (paragraph-separate (concat "[[:blank:]]*" slashes
                                                  "\\(?:" paragraph-separate
                                                  "\\)")))
                  (fill-region start end justify :preserve-spaces)
                  (indent-region start end))))

             ;; Star-style comment
             ((re-search-forward "/\\*\\{2,\\} *" nil t)
              (let* ((edges (swift-mode:-fix-up-star-comment-edges))
                     (start (car edges))
                     (end (cdr edges)))
                (fill-region start end justify :preserve-spaces)
                (indent-region start end))))))

        ;; Make sure `fill-paragraph' does not undo our work.
        t))))

(provide 'swift-mode-fill)

;;; swift-mode-fill.el ends here
