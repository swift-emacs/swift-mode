;;; swift-mode-fill.el --- Major-mode for Apple's Swift programming language, paragraph filling. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Josh Caswell, taku0

;; Author: Josh Caswell (https://github.com/woolsweater)
;;         taku0 (http://github.com/taku0)

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
(require 'swift-mode-beginning-of-defun)

(defcustom swift-mode:fill-paragraph-entire-comment-or-string nil
  "When non-nil, `fill-paragraph' fills entire comment block or string."
  :type 'boolean
  :group 'swift
  :safe #'booleanp)

(defconst swift-mode:doc-comment-paragraph-start
  (let* ((list-marker '(or (any ?- ?+ ?*) (seq (* (any "0-9")) (any ".)"))))
         (list-item `(seq ,list-marker (or blank eol)))
         (atx-heading '(seq (+ "#") blank)))
    (rx-to-string `(seq
                    (* blank)
                    (or ,list-item ,atx-heading))))
  "Regex to match start of paragraphs in documentation comments.

This is used by `swift-mode:fill-forward-paragraph' to extend
`paragraph-start' such that the built-in fill functions recognize
these elements as the beginnings of their own paragraphs.")

(defconst swift-mode:doc-comment-paragraph-separate
  (let* ((code-fence '(seq (or "```" "~~~") (* not-newline)))
         (thematic-break '(or (>= 3 "-" (* blank))
                              (>= 3 "_" (* blank))
                              (>= 3 "*" (* blank))))
         (setext-heading-underline '(or (* "=") (* "-"))))
    (rx-to-string `(seq
                    (* blank)
                    (or
                     ,code-fence
                     ,thematic-break
                     ,setext-heading-underline)
                    (* blank)
                    eol)))
  "Regex to match paragraph separators in documentation comments.

This is used by `swift-mode:fill-forward-paragraph' to extend
`paragraph-separate'.")

(defsubst swift-mode:find-single-line-comment-edges ()
  "Return start and end of a single-line comment block.

A single-line comment block is a continuous lines with same \"comment level\".

Comment level is:

- Number of slashes after indentation, if the line contains characters
  other than slashes and spaces.

- Comment level of the following line if the line contains only slashes
  and spaces, and the following line is a single line comment and it have
  fewer or equal number of slashes.

- Comment level of the preceding line if the line contains only slashes
  and spaces, and the preceding line is a single line comment and it have
  fewer or equal number of slashes.

- Number of slashes after indentation, otherwise.


Examples:

Code comment after documentation comment:

/// paragraph 1: aaa bbb
/// ccc.
// paragraph 2: aaa bbb
// ccc.


Comment box:

///////////////////////
// paragraph 1: aaa bbb
// ccc.
//
// paragraph 2: aaa bbb
// ccc.
///////////////////////


Code comment before and after comment box:

// paragraph 1: aaa bbb
// ccc.
////////////////////////////
/// paragraph 2: aaa bbb
/// ccc.
////////////////////////////
// paragraph 3: aaa bbb
// ccc.

Return tuple (START . END) where START is the point before the first slash of
the block, and END is the end of the last comment, excluding the last line
break if any.

Point may be anywhere in a single-line comment when this is called."
  (let* ((current-comment-level (swift-mode:single-line-comment-level))
         (start
          (save-excursion
            (while (and (not (bobp))
                        (= (swift-mode:single-line-comment-level)
                           current-comment-level))
              (forward-line -1))
            (unless (= (swift-mode:single-line-comment-level)
                       current-comment-level)
              (forward-line 1))
            (back-to-indentation)
            (point)))
         (end
          (save-excursion
            (while (and (not (eobp))
                        (= (swift-mode:single-line-comment-level)
                           current-comment-level))
              (when (/= (forward-line 1) 0)
                (goto-char (point-max))))
            (when (bolp)
              (backward-char))
            (point))))
    (cons start end)))

(defun swift-mode:single-line-comment-level (&optional search-direction)
  "Return comment level of the current line.

See `swift-mode:find-single-line-comment-edges' for details.

If SEARCH-DIRECTION is `backward', search only backward.
If SEARCH-DIRECTION is `forward', search only forward.

Return 1.0e+INF if the line doesn't start with a single-line comment."
  (save-excursion
    (save-match-data
      (back-to-indentation)
      (if (looking-at "//+")
          (let ((number-of-slashes (- (match-end 0) (match-beginning 0))))
            (if (looking-at "//+\\s *$")
                ;; Only slashes.
                (or
                 (let ((following-comment-level
                        (save-excursion
                          (if (and (zerop (forward-line 1))
                                   (bolp)
                                   (not (eq search-direction 'backward)))
                              (swift-mode:single-line-comment-level 'forward)
                            1.0e+INF))))
                   (and (<= following-comment-level number-of-slashes)
                        following-comment-level))
                 (let ((preceding-comment-level
                        (save-excursion
                          (if (and (zerop (forward-line -1))
                                   (not (eq search-direction 'forward)))
                              (swift-mode:single-line-comment-level 'backward)
                            1.0e+INF))))
                   (and (<= preceding-comment-level number-of-slashes)
                        preceding-comment-level))
                 number-of-slashes)
              number-of-slashes))
        ;; Not a comment
        1.0e+INF))))

(defun swift-mode:fill-paragraph (justify)
  "Fill paragraph in Swift code.

JUSTIFY is as the argument of the same name in `fill-region'.

If `swift-mode:fill-paragraph-entire-comment-or-string' is non-nil, fill entire
comment rather than a paragraph.

Determine which style of comment is at or around point and does preliminary
cleanup as needed (the built-in fill functions do not handle the '/**' style of
comment particularly well)."
  ;; TODO A leading star on an empty line screws up paragraph calculation.
  ;; TODO Handle trailing comments.
  (save-excursion
    (save-match-data
      (skip-syntax-backward " ")
      (let ((chunk (or (swift-mode:chunk-after)
                       (and (looking-at "\\s *\\(/[/*]\\|#*\"\"\"\\)")
                            (swift-mode:chunk-after (match-end 0)))
                       (save-excursion
                         (skip-chars-backward "#")
                         (skip-chars-backward "\"")
                         (swift-mode:chunk-after))
                       (and (eq (char-before) ?/)
                            (save-excursion
                              (backward-char)
                              (skip-chars-backward "*")
                              (swift-mode:chunk-after))))))
        (cond
         ;; Single-line comment
         ((swift-mode:chunk:single-line-comment-p chunk)
          (let* ((edges (swift-mode:find-single-line-comment-edges))
                 (start (car edges))
                 (end (copy-marker (cdr edges))))
            (if swift-mode:fill-paragraph-entire-comment-or-string
                (fill-region start end justify)
              (let ((fill-paragraph-function nil))
                (fill-paragraph justify)))
            (indent-region start end)
            (set-marker end nil)))

         ;; Multiline comment or string
         ((or (swift-mode:chunk:multiline-comment-p chunk)
              (swift-mode:chunk:multiline-string-p chunk))
          (let* ((start (swift-mode:chunk:start chunk))
                 (end (swift-mode:chunk:end chunk)))
            (if swift-mode:fill-paragraph-entire-comment-or-string
                (fill-region start end justify)
              (let ((fill-paragraph-function nil))
                (fill-paragraph justify)))))
         ;; Otherwise; do nothing
         (t
          nil)))
      t)))

(defun swift-mode:fill-region-as-paragraph-advice
    (fill-region-as-paragraph from to &rest args)
  "Advice function for `fill-region-as-paragraph'.

FILL-REGION-AS-PARAGRAPH is the original function, and FROM, TO, and ARGS are
original arguments.

Fix up multiline comments.

- When the region contains other than one multline comment, fill normally:

  foo() /* abc def ghi */
  ↓
  foo() /* abc
  def ghi */

- Otherwise and when the region fits one line, fill as a line:

  /*
    abc
    def
  */
  ↓
  /* abc def */

- Otherwise and when the region was one line, insert breaks before and after
  the contents:

  /* abc def ghi */
  ↓
  /*
    abc def
    ghi
  */

- Otherwise, keep line breaks around the contents and fill the contents:

  /* abc def ghi
  */
  ↓
  /* abc def
     ghi
  */"
  (if (eq major-mode 'swift-mode)
      (let* ((chunk (save-excursion
                      (save-match-data
                        (goto-char from)
                        (or (swift-mode:chunk-after)
                            (and (looking-at "\\s *\\(/[/*]\\|#*\"\"\"\\)")
                                 (swift-mode:chunk-after (match-end 0)))))))
             comment-start-pos
             comment-end-pos
             one-line
             have-break-after-open-delimiter
             have-break-before-close-delimiter
             contents-start
             contents-end
             result)
        (if (swift-mode:chunk:multiline-comment-p chunk)
            (progn
              (setq comment-start-pos (swift-mode:chunk:start chunk))
              (setq comment-end-pos (swift-mode:chunk:end chunk))
              ;; Is filling the entire comment?
              (if (and (member (save-excursion
                                 (goto-char from)
                                 (skip-syntax-forward " >")
                                 (point))
                               (list comment-start-pos
                                     (save-excursion
                                       (goto-char comment-start-pos)
                                       (forward-char)
                                       (skip-chars-forward "*")
                                       (skip-syntax-forward " >")
                                       (point))))
                       (member (save-excursion
                                 (goto-char to)
                                 (skip-syntax-backward " >")
                                 (point))
                               (list comment-end-pos
                                     (save-excursion
                                       (goto-char comment-end-pos)
                                       (backward-char)
                                       (skip-chars-backward "*")
                                       (skip-syntax-backward " >")
                                       (point)))))
                  (progn
                    (setq one-line (swift-mode:same-line-p
                                    comment-start-pos
                                    comment-end-pos))
                    (setq have-break-after-open-delimiter
                          (save-excursion
                            (goto-char comment-start-pos)
                            (forward-char)
                            (skip-chars-forward "*")
                            (skip-syntax-forward " ")
                            (eolp)))
                    (setq have-break-before-close-delimiter
                          (save-excursion
                            (goto-char comment-end-pos)
                            (backward-char)
                            (skip-chars-backward "*")
                            (skip-syntax-backward " ")
                            (bolp)))
                    (setq comment-start-pos (copy-marker comment-start-pos))
                    (setq comment-end-pos (copy-marker comment-end-pos))
                    (setq result (apply fill-region-as-paragraph from to args))
                    ;; If the entire comment fits in one line, do nothing.
                    ;; Otherwise, insert line breaks before/after the contents
                    ;; if necessary.  See the documentation comment for details.
                    (unless (swift-mode:same-line-p
                             comment-start-pos
                             comment-end-pos)
                      (save-excursion
                        (goto-char comment-start-pos)
                        (forward-char)
                        (skip-chars-forward "*")
                        (skip-syntax-forward " ")
                        (when (and
                               (or one-line have-break-after-open-delimiter)
                               (not (eolp)))
                          (delete-horizontal-space)
                          (insert-and-inherit "\n")
                          (indent-according-to-mode))
                        (setq contents-start (point)))
                      (save-excursion
                        (goto-char comment-end-pos)
                        (backward-char)
                        (skip-chars-backward "*")
                        (skip-syntax-backward " ")
                        (setq contents-end (point))
                        (when (and
                               (or one-line have-break-before-close-delimiter)
                               (not (bolp)))
                          (delete-horizontal-space)
                          (insert-and-inherit "\n")
                          (indent-according-to-mode)))
                      (setq result (apply fill-region-as-paragraph
                                          contents-start
                                          contents-end
                                          args)))
                    (set-marker comment-start-pos nil)
                    (set-marker comment-end-pos nil)
                    result)
                (apply fill-region-as-paragraph from to args)))
          (apply fill-region-as-paragraph from to args)))
    (apply fill-region-as-paragraph from to args)))

(defun swift-mode:install-fill-region-as-paragraph-advice ()
  "Install advice around `fill-region-as-paragraph'."
  (advice-add 'fill-region-as-paragraph
              :around
              #'swift-mode:fill-region-as-paragraph-advice))


(defun swift-mode:current-fill-column-advice (current-fill-column)
  "Advice function for `current-fill-column'.

CURRENT-FILL-COLUMN is the original function.

Use `comment-fill-column' as `fill-column' when filling inside a comment."
  (if (and (eq major-mode 'swift-mode) comment-fill-column)
      (let* ((chunk (save-excursion
                      (save-match-data
                        (or (swift-mode:chunk-after)
                            (and (looking-at "\\s *\\(/[/*]\\|#*\"\"\"\\)")
                                 (swift-mode:chunk-after (match-end 0)))))))
             (fill-column
              (if (swift-mode:chunk:comment-p chunk)
                  comment-fill-column
                fill-column)))
        (funcall current-fill-column))
    (funcall current-fill-column)))

(defun swift-mode:install-current-fill-column-advice ()
  "Install advice around `current-fill-column'."
  (advice-add 'current-fill-column
              :around
              #'swift-mode:current-fill-column-advice))

(defun swift-mode:fill-forward-paragraph (arg)
  "Forward ARG paragraphs for filling.

Returns the count of paragraphs left to move."
  (if (< arg 0)
      (swift-mode:fill-backward-paragraph (- arg))
    (let ((done nil))
      (while (and (< 0 arg)
                  (not done))
        (setq done (not (swift-mode:fill-skip-paragraph-1 'forward)))
        (unless done (setq arg (1- arg)))))
    arg))

(defun swift-mode:fill-backward-paragraph (arg)
  "Backward ARG paragraphs for filling.

Returns the count of paragraphs left to move."
  (if (< arg 0)
      (swift-mode:fill-forward-paragraph (- arg))
    (let ((done nil))
      (while (and (< 0 arg)
                  (not done))
        (setq done (not (swift-mode:fill-skip-paragraph-1 'backward)))
        (unless done (setq arg (1- arg)))))
    arg))

(defun swift-mode:fill-skip-paragraph-1 (direction)
  "Skip a paragraph for filling.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (save-match-data
    ;; Skip whitespaces and line breaks.
    (if (eq direction 'backward)
        (skip-syntax-backward " >")
      (skip-syntax-forward " >"))
    (let ((chunk (or (swift-mode:chunk-after)
                     (and (looking-at "/[/*]\\|#*\"\"\"")
                          (swift-mode:chunk-after (match-end 0))))))
      (cond
       ((swift-mode:chunk:single-line-comment-p chunk)
        (swift-mode:fill-skip-paragraph-in-single-line-comment
         chunk
         direction))
       ((swift-mode:chunk:multiline-comment-p chunk)
        (swift-mode:fill-skip-paragraph-in-multiline-comment
         chunk
         direction))
       ((swift-mode:chunk:string-p chunk)
        (swift-mode:fill-skip-paragraph-in-string
         chunk
         direction))
       (t
        (swift-mode:fill-skip-paragraph-in-code direction))))))

(defun swift-mode:fill-skip-paragraph-in-single-line-comment (chunk direction)
  "Skip a paragraph in a single line comment for filling.

CHUNK is the comment.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (let* ((backward (eq direction 'backward))
         (pos (point))
         (comment-level (save-excursion
                          (goto-char (swift-mode:chunk:start chunk))
                          (swift-mode:single-line-comment-level)))
         (slashes (make-string comment-level ?/))
         (edges (swift-mode:find-single-line-comment-edges))
         ;; Factor the comment markers into paragraph recognition
         (paragraph-start (concat
                           "[[:blank:]]*"
                           slashes "/*"
                           "\\(?:"
                           swift-mode:doc-comment-paragraph-start
                           "\\|"
                           paragraph-start
                           "\\)"))
         (paragraph-separate (concat "[[:blank:]]*"
                                     slashes "/*"
                                     "[[:blank:]]*"
                                     "\\(?:"
                                     swift-mode:doc-comment-paragraph-separate
                                     "\\|"
                                     paragraph-separate
                                     "\\)")))
    (if backward (backward-paragraph) (forward-paragraph))
    (when (and (< (point) (car edges))
               (< (car edges) pos))
      (goto-char (car edges)))
    (when (and (< (cdr edges) (point))
               (< pos (cdr edges)))
      (goto-char (cdr edges)))
    (/= pos (point))))

(defun swift-mode:fill-skip-paragraph-in-multiline-comment (chunk direction)
  "Skip a paragraph in a multiline comment for filling.

CHUNK is the comment.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (swift-mode:fill-skip-paragraph-in-multiline-chunk
   chunk
   direction
   "\\s */\\*+\\s *$\\|\\s *\\*+/\\s *$"
   (if (eq direction 'backward)
       (lambda ()
         (forward-char)
         (skip-chars-forward "*")
         (skip-syntax-forward " "))
     (lambda ()
       (when (eq (char-before) ?/)
         (backward-char)
         (skip-chars-backward "*"))
       (skip-syntax-backward " ")))))

(defun swift-mode:fill-skip-paragraph-in-string (chunk direction)
  "Skip a paragraph in a string for filling.

CHUNK is the comment.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (swift-mode:fill-skip-paragraph-in-multiline-chunk
   chunk
   direction
   "\\s *#*\"+\\s *$\\|\\s *\"+#*\\s *$"
   (if (eq direction 'backward)
       (lambda ()
         (skip-chars-forward "#")
         (skip-chars-forward "\"")
         (skip-syntax-forward " "))
     (lambda ()
       (skip-chars-backward "#")
       (skip-chars-backward "\"")
       (skip-syntax-backward " ")))))

(defun swift-mode:fill-skip-paragraph-in-multiline-chunk
    (chunk direction extra-paragraph-separate skip-delimiter)
  "Skip a paragraph in a multiline comment or string for filling.

CHUNK is the comment or string.

EXTRA-PARAGRAPH-SEPARATE is additional `paragraph-separate' regexp.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

SKIP-DELIMITER is a function that skips delimiter in opposite direction.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (let* ((backward (eq direction 'backward))
         (pos (point))
         (limit (save-excursion
                  (goto-char (if backward
                                 (swift-mode:chunk:start chunk)
                               (swift-mode:chunk:end chunk)))
                  (funcall skip-delimiter)
                  (if backward
                      (skip-syntax-forward " ")
                    (skip-syntax-backward " "))
                  (if backward
                      (min pos (point))
                    (max pos (point)))))
         (min (if backward limit pos))
         (max (if backward pos limit))
         (paragraph-start (concat
                           swift-mode:doc-comment-paragraph-start
                           "\\|"
                           paragraph-start))
         (paragraph-separate (concat extra-paragraph-separate
                                     "\\|"
                                     swift-mode:doc-comment-paragraph-separate
                                     "\\|"
                                     paragraph-separate)))
    (forward-paragraph (if backward -1 1))
    (when (< (point) min)
      (if (< min pos)
          (goto-char min)
        (goto-char (swift-mode:chunk:start chunk))
        (unless (zerop (fill-forward-paragraph -1))
          (goto-char pos))))
    (when (< max (point))
      (if (< pos max)
          (goto-char max)
        (goto-char (swift-mode:chunk:end chunk))
        (unless (zerop (fill-forward-paragraph 1))
          (goto-char pos))))
    (/= (point) pos)))

(defun swift-mode:fill-skip-paragraph-in-code (direction)
  "Skip a paragraph in a code for filling.

If DIRECTION is `backward', skip backward.  Otherwise, skip forward.

Return non-nil if skipped a paragraph.  Return nil otherwise."
  (let ((pos (point))
        (done nil))
    (while (not done)
      (if (or (not (zerop (forward-line (if (eq direction 'backward) -1 1))))
              (eobp)
              (bobp))
          (progn
            (setq done t)
            ;; When no paragraph is found, `fill-region' expects to keep
            ;; position when going backward while go forward anyway when going
            ;; forward.
            (when (eq direction 'backward)
              (goto-char pos)))
        (back-to-indentation)
        (when (or (looking-at "/[/*]\\|#*\"\"\"")
                  (swift-mode:chunk-after))
          (setq done t)
          (when (eq direction 'backward)
            (end-of-line))
          (swift-mode:fill-skip-paragraph-1 direction))))
    (/= pos (point))))

(defun swift-mode:do-auto-fill ()
  "Do auto fill at point.

Do nothing except in a comment.

If point is inside a muitiline style comment (slash-star style comment) which
is actually in single line, insert line breaks before and after the contents,
then call `do-auto-fill'.

Example:

/* aaa bbb ccc */
↓
/*
  aaa bbb
  ccc
*/"
  (let ((current-fill-column (current-fill-column))
        (current-justification (current-justification))
        chunk)
    (if (or (null current-justification)
            (null fill-column)
            (and (eq current-justification 'left)
                 (<= (current-column) current-fill-column))
            (null (setq chunk (swift-mode:chunk-after)))
            (not (swift-mode:chunk:comment-p chunk)))
        ;; Do nothing.
        nil
      (when (swift-mode:chunk:multiline-comment-p chunk)
        (let ((comment-start-pos (swift-mode:chunk:start chunk))
              (comment-end-pos (swift-mode:chunk:end chunk)))
          (when (swift-mode:same-line-p comment-start-pos comment-end-pos)
            (save-excursion
              (goto-char comment-start-pos)
              (forward-char)
              (skip-chars-forward "*")
              (delete-horizontal-space)
              (insert-and-inherit "\n")
              (indent-according-to-mode))
            (save-excursion
              (goto-char comment-end-pos)
              (backward-char)
              (skip-chars-backward "*")
              (skip-syntax-backward " ")
              (delete-horizontal-space)
              (insert-and-inherit "\n")
              (indent-according-to-mode)))))
      (do-auto-fill))))

(provide 'swift-mode-fill)

;;; swift-mode-fill.el ends here
