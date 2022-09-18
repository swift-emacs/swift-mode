;;; swift-mode-test-fill.el --- Test for swift-mode: filling -*- lexical-binding: t -*-
;; Copyright (C) 2016, 2022 taku0, Josh Caswell

;; Authors: taku0 (https://github.com/taku0)
;;        Josh Caswell (https://github.com/woolsweater)

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

;; Test for swift-mode: paragraph fill
;; Execute swift-mode:run-test:fill interactively or in batch mode.

;;; Code:

(require 'swift-mode)
(require 'swift-mode-test)
(require 'swift-mode-fill)
(require 'diff)

(defun swift-mode:run-test:fill
    (&optional error-buffer error-counts progress-reporter)
  "Run paragraph fill tests for `swift-mode'.

ERROR-BUFFER is the buffer to collect errors.  ERROR-COUNTS is an
association list holding counts of errors, updated destructively.
PROGRESS-REPORTER is a `progress-reporter' used when the tests are run
interactively."
  (interactive)
  (if (not swift-mode:test:running)
      (swift-mode:run-test '(swift-mode:run-test:fill))
    (swift-mode:test-fill-region-as-paragraph error-buffer error-counts)
    (let (lines)
      (setq default-directory
            (concat (file-name-as-directory swift-mode:test:basedir)
                    (file-name-as-directory "swift-files")
                    "fill"))
      (dolist (swift-file (file-expand-wildcards "*.swift"))
        (redisplay)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (insert-file-contents-literally swift-file)
          (let ((coding-system-for-read 'utf-8))
            (decode-coding-inserted-region (point-min) (point-max) swift-file))
          (swift-mode)
          (syntax-propertize (point-max))
          (setq lines (swift-mode:test-fill:parse-fill-test)))
        (dolist (mode '(break join))
          (swift-mode:test-fill:test-fill-region
           swift-file
           lines
           mode
           error-buffer
           error-counts)
          (swift-mode:test-fill:test-fill-paragraph
           swift-file
           lines
           mode
           error-buffer
           error-counts
           progress-reporter))))))

(defun swift-mode:test-fill-region-as-paragraph (error-buffer error-counts)
  "Run tests for `fill-region-as-paragraph'.

See `swift-mode:run-test:fill' for ERROR-BUFFER and ERROR-COUNTS."
  ;; Sinle-line comments, insert breaks:
  (swift-mode:do-test-fill-region-as-paragraph
   "// abc def ghi\n"
   "// abc def\n// ghi\n"
   8
   12
   error-buffer
   error-counts)

  ;; Sinle-line comments, delete breaks:
  (swift-mode:do-test-fill-region-as-paragraph
   "// abc\n// def\n// ghi\n"
   "// abc def\n// ghi\n"
   8
   12
   error-buffer
   error-counts)

  ;; Multiline comments, insert breaks:
  (swift-mode:do-test-fill-region-as-paragraph
   "/* abc def ghi jkl */\n"
   "/*\n abc def ghi\n jkl\n */\n"
   8
   12
   error-buffer
   error-counts)

  ;; Multiline comments, delete breaks:
  (swift-mode:do-test-fill-region-as-paragraph
   "/*\n abc\n def\n ghi\n */\n"
   "/*\n abc def ghi\n */\n"
   8
   12
   error-buffer
   error-counts)

  ;; Multiline comments, to one-line:
  (swift-mode:do-test-fill-region-as-paragraph
   "/*\n abc\n def\n ghi\n */\n"
   "/* abc def ghi */\n"
   8
   80
   error-buffer
   error-counts)

  ;; Multiline comments, keep line break after open delimiter:
  (swift-mode:do-test-fill-region-as-paragraph
   "/*\n abc\n def\n ghi */\n"
   "/*\n abc def ghi */\n"
   8
   12
   error-buffer
   error-counts)

  ;; Multiline comments, keep line break before close delimiter:
  (swift-mode:do-test-fill-region-as-paragraph
   "/* abc\n def\n ghi\n */\n"
   "/* abc def\n   ghi\n */\n"
   8
   12
   error-buffer
   error-counts))

(defun swift-mode:do-test-fill-region-as-paragraph
    (input
     expected
     fill-column-for-test
     comment-fill-column-for-test
     error-buffer
     error-counts)
  "Run a test for `fill-region-as-paragraph'.

INPUT is a text before filling.

EXPECTED is the expected result.

FILL-COLUMN-FOR-TEST and COMMENT-FILL-COLUMN-FOR-TEST is used for `fill-column'
and `comment-fill-column' respectively.

See `swift-mode:run-test:fill' for ERROR-BUFFER and ERROR-COUNTS."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (insert input)
    (swift-mode)
    (syntax-propertize (point-max))
    (let ((fill-column fill-column-for-test)
          (comment-fill-column comment-fill-column-for-test))
      (fill-region-as-paragraph (point-min) (point-max)))
    (let* ((status (if (equal (buffer-string) expected)
                       'ok
                     (swift-mode:show-error
                      error-buffer "swift-mode-test-fill.el" 0
                      "error"
                      (concat "`fill-region-as-paragraph' failed\n"
                              "Expected:\n```\n" expected "```\n\n"
                              "Actual:\n```\n" (buffer-string) "```"))
                     'error))
           (count-assoc (assq status error-counts)))
      (setcdr count-assoc (1+ (cdr count-assoc))))))

(defun swift-mode:test-fill:parse-fill-test ()
  "Parse the current buffer as a test file and return its structure.

The result is list of elements, which is one of:

- non-paragraph line, (literal STRING), where STRING is the line
  excluding a line break,

- paragraph, (paragraph PREFIX BLOCK-BOUNDARY-TYPE), where PREFIX is the
  fill prefix and BLOCK-BOUNDARY-TYPE is either nil, `start', or `end', or

- list item, (list-item PREFIX).

- heading, (heading PREFIX)."
  (save-excursion
    (goto-char (point-min))
    (let ((result ()))
      (while (not (eobp))
        (push
         (cond
          ((looking-at "\\(.*\\)swift-mode:test:paragraph")
           (list 'paragraph
                 (match-string-no-properties 1)
                 (cond
                  ((looking-at ".*swift-mode:test:start-block")
                   'start)
                  ((looking-at ".*swift-mode:test:end-block")
                   'end)
                  (t
                   nil))))
          ((looking-at "\\(.*\\)swift-mode:test:list-item")
           (list 'list-item (match-string-no-properties 1)))
          ((looking-at "\\(.*\\)swift-mode:test:heading")
           (list 'heading (match-string-no-properties 1)))
          (t
           (list 'literal
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))))
         result)
        (forward-line))
      (reverse result))))

(defun swift-mode:test-fill:test-fill-region
    (filename lines mode error-buffer error-counts)
  "Run tests for `fill-region'.

FILENAME is the name of test file.

LINES is the parsed lines of the test file.
See `swift-mode:test-fill:parse-fill-test' for details.

MODE is either `break' or `join'.  If it is `break', test breaking long lines.
If it is `join' test joining short lines.

See `swift-mode:run-test:fill' for ERROR-BUFFER and ERROR-COUNTS."
  (let (regions
        expected
        actual)
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (swift-mode)
      (setq regions (nth 0 (swift-mode:test-fill:insert-test lines mode)))
      (syntax-propertize (point-max))
      (dolist (region (reverse regions))
        (fill-region-as-paragraph (nth 0 region) (nth 1 region)))
      (setq expected (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (swift-mode)
      (swift-mode:test-fill:insert-test lines mode)
      (syntax-propertize (point-max))
      (fill-region (point-min) (point-max))
      (setq actual (buffer-substring-no-properties (point-min) (point-max))))
    (let* ((status (if (equal actual expected)
                       'ok
                     (swift-mode:show-error
                      error-buffer filename 0
                      "error"
                      (concat "`fill-region' failed\n"
                              (swift-mode:test-fill:diff-strings
                               expected
                               actual)))
                     'error))
           (count-assoc (assq status error-counts)))
      (setcdr count-assoc (1+ (cdr count-assoc))))))

(defun swift-mode:test-fill:test-fill-paragraph
    (filename
     lines
     mode
     error-buffer
     error-counts
     progress-reporter)
  "Run tests for `fill-paragraph'.

FILENAME is the name of test file.

LINES is the parsed lines of the test file.
See `swift-mode:test-fill:parse-fill-test' for details.

MODE is either `break' or `join'.  If it is `break', test breaking long lines.
If it is `join' test joining short lines.

See `swift-mode:run-test:fill' for ERROR-BUFFER, ERROR-COUNTS, and
PROGRESS-REPORTER."
  (let (regions-and-blocks
        regions
        blocks
        original
        expected
        (point-placements '(beginning-of-line
                            after-indent
                            after-first-slash
                            after-slashes
                            end-of-line)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (swift-mode)
      (setq regions-and-blocks (swift-mode:test-fill:insert-test lines mode))
      (setq regions (nth 0 regions-and-blocks))
      (setq blocks (nth 1 regions-and-blocks))
      (setq original (buffer-string))
      (dolist (region regions)
        (delete-region (point-min) (point-max))
        (insert original)
        (syntax-propertize (point-max))
        (fill-region-as-paragraph (nth 0 region) (nth 1 region))
        (setq expected (buffer-substring-no-properties (point-min) (point-max)))
        (dolist (point-placement point-placements)
          (when (not noninteractive)
            (progress-reporter-update progress-reporter))
          (swift-mode:test-fill:do-test-fill-paragraph
           filename
           original
           expected
           region
           point-placement
           mode
           nil
           error-buffer
           error-counts)))
      (dolist (block blocks)
        (delete-region (point-min) (point-max))
        (insert original)
        (syntax-propertize (point-max))
        (fill-region (nth 0 block) (nth 1 block))
        (setq expected (buffer-substring-no-properties (point-min) (point-max)))
        (dolist (point-placement point-placements)
          (when (not noninteractive)
            (progress-reporter-update progress-reporter))
          (swift-mode:test-fill:do-test-fill-paragraph
           filename
           original
           expected
           block
           point-placement
           mode
           t
           error-buffer
           error-counts))))))

(defun swift-mode:test-fill:do-test-fill-paragraph (filename
                                                    original
                                                    expected
                                                    region
                                                    point-placement
                                                    mode
                                                    entire-comment
                                                    error-buffer
                                                    error-counts)
  "Run single test for `fill-paragraph'.

FILENAME is the name of test file.

ORIGINAL is a text before filling.

EXPECTED is the expected result.

REGION is the region to fill.

POINT-PLACEMENT designates where to put the point before filling.  It must be
one of the following:
- `beginning-of-line'
- `after-indent'
- `after-first-slash'
- `after-slashes'
- `end-of-line'
- `end-of-region'

MODE is either `break' or `join'.  If it is `break', test breaking long lines.
If it is `join' test joining short lines.

ENTIRE-COMMENT is used for `swift-mode:fill-paragraph-entire-comment-or-string'.

See `swift-mode:run-test:fill' for ERROR-BUFFER, ERROR-COUNTS."
  (let (actual)
    (delete-region (point-min) (point-max))
    (insert original)
    (syntax-propertize (point-max))
    (cond
     ((eq point-placement 'beginning-of-line)
      (goto-char (nth 0 region)))
     ((eq point-placement 'after-indent)
      (goto-char (nth 0 region))
      (back-to-indentation))
     ((eq point-placement 'after-first-slash)
      (goto-char (nth 0 region))
      (back-to-indentation)
      (when (eq (char-after) ?/)
        (forward-char)))
     ((eq point-placement 'after-slashes)
      (goto-char (nth 0 region))
      (back-to-indentation)
      (skip-chars-forward "/"))
     ((eq point-placement 'end-of-line)
      (goto-char (nth 0 region))
      (end-of-line))
     ((eq point-placement 'end-of-region)
      (goto-char (1- (nth 1 region)))))
    (let ((swift-mode:fill-paragraph-entire-comment-or-string entire-comment))
      (fill-paragraph))
    (setq actual (buffer-substring-no-properties (point-min) (point-max)))
    (let* ((status
            (if (equal actual expected)
                'ok
              (swift-mode:show-error
               error-buffer filename (nth 2 region)
               "error"
               (concat "`fill-paragraph' failed\n"
                       "point-placement: " (symbol-name point-placement) "\n"
                       "mode: " (prin1-to-string mode) "\n"
                       "entire-comment: " (prin1-to-string entire-comment) "\n"
                       (swift-mode:test-fill:diff-strings
                        expected
                        actual)))
              'error))
           (count-assoc (assq status error-counts)))
      (setcdr count-assoc (1+ (cdr count-assoc))))))

(defun swift-mode:test-fill:insert-test (lines mode)
  "Insert parsed lines at point.

LINES is the parsed lines of the test file.
See `swift-mode:test-fill:parse-fill-test' for details.

MODE is either `break' or `join'.  If it is `break', test breaking long lines.
If it is `join' test joining short lines."
  (let (regions
        blocks
        start
        block-start
        (line-number 0))
    (dolist (line lines)
      (setq line-number (1+ line-number))
      (cond
       ((eq (car line) 'literal)
        (insert (nth 1 line) "\n"))
       ((eq (car line) 'paragraph)
        (setq start (point))
        (when (eq (nth 2 line) 'start)
          (setq block-start (point)))
        (swift-mode:test-fill:insert-paragraph
         (nth 1 line)
         (nth 1 line)
         mode)
        (push (list start (point) line-number) regions)
        (when (eq (nth 2 line) 'end)
          (push (list block-start (point) line-number) blocks)))
       ((eq (car line) 'list-item)
        (setq start (point))
        (swift-mode:test-fill:insert-paragraph
         (concat (nth 1 line) "- ")
         (nth 1 line)
         mode)
        (push (list start (point) line-number) regions))
       ((eq (car line) 'heading)
        (setq start (point))
        (swift-mode:test-fill:insert-paragraph
         (concat (nth 1 line) "## ")
         (nth 1 line)
         mode)
        (push (list start (point) line-number) regions))))
    (list (reverse regions) (reverse blocks))))

(defun swift-mode:test-fill:insert-paragraph (first-line-prefix prefix mode)
  "Insert a test paragraph at point.

FIRST-LINE-PREFIX is inserted before the first line, while PREFIX is inserted
before other lines.

If MODE is `join', insert short multiple lines.  If MODE is `break', insert a
long line."
  (cond
   ((eq mode 'join)
    (insert first-line-prefix "aaa\n")
    (dotimes (_ 100)
      (insert prefix "aaa\n")))
   ((eq mode 'break)
    (insert first-line-prefix)
    (dotimes (_ 100)
      (insert "aaa "))
    (insert "\n"))))

(defun swift-mode:test-fill:diff-strings (expected actual)
  "Return difference of EXPECTED and ACTUAL."
  (let (expected-buffer actual-buffer)
    (with-temp-buffer
      (rename-buffer "expected" t)
      (setq expected-buffer (current-buffer))
      (insert expected)
      (with-temp-buffer
        (rename-buffer "actual" t)
        (setq actual-buffer (current-buffer))
        (insert actual)
        (with-temp-buffer
          (diff-no-select expected-buffer actual-buffer nil t (current-buffer))
          (buffer-string))))))

(provide 'swift-mode-test-fill)

;;; swift-mode-test-fill.el ends here
