;;; swift-mode-test-beginning-of-defun.el --- Test for swift-mode: beginning-of-defun -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 taku0

;; Author: taku0 (http://github.com/taku0)

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

;; Test for swift-mode: beginning-of-defun.
;; Execute swift-mode:run-test:beginning-of-defun interactively or in batch
;; mode.

;;; Code:

(require 'swift-mode)
(require 'swift-mode-beginning-of-defun)
(require 'seq)

(defun swift-mode:run-test:beginning-of-defun
    (&optional error-buffer error-counts progress-reporter)
  "Run `beginning-of-defun' test for `swift-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors.  Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)
  (if (not swift-mode:test:running)
      (swift-mode:run-test '(swift-mode:run-test:beginning-of-defun))
    (let ((current-line 0)
          expected-positions
          expected-positions-desc
          expected-positions-asc
          test-parameters)
      (setq default-directory
            (concat (file-name-as-directory swift-mode:test:basedir)
                    (file-name-as-directory "swift-files")
                    "beginning-of-defun"))
      (dolist (swift-file (file-expand-wildcards "*.swift"))
        (redisplay)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (insert-file-contents-literally swift-file)
          (swift-mode)
          (setq expected-positions
                (swift-mode:parse-beginning-of-defun-test-file))
          (setq expected-positions-desc
                (mapcar (lambda (p)
                          (list
                           (nth 0 p)
                           (nth 2 p)
                           (nth 4 p)))
                        expected-positions))
          (setq expected-positions-asc
                (mapcar (lambda (p)
                          (list
                           (nth 0 p)
                           (nth 1 p)
                           (nth 3 p)))
                        (reverse expected-positions)))
          (setq test-parameters
                (list
                 (list
                  expected-positions-desc
                  #'<
                  (lambda ()
                    (swift-mode:beginning-of-defun)
                    (skip-syntax-forward " "))
                  'beginning-of-defun)
                 (list
                  expected-positions-asc
                  #'>
                  #'swift-mode:end-of-defun
                  'end-of-defun)
                 (list
                  expected-positions-desc
                  #'<
                  (lambda ()
                    (swift-mode:backward-sentence)
                    (skip-syntax-forward " "))
                  '(beginning-of-sentence beginning-of-defun))
                 (list
                  expected-positions-asc
                  #'>
                  #'swift-mode:forward-sentence
                  '(end-of-sentence end-of-defun))))
          (setq current-line 0)
          (while (not (eobp))
            (when (not noninteractive)
              (progress-reporter-update progress-reporter))
            (setq current-line (1+ current-line))
            (when (looking-at ".*//.*swift-mode:test:eval\\(.*\\)")
              (eval-region (match-beginning 1) (match-end 1)))

            (dolist (test-parameter test-parameters)
              (let* ((status (apply
                              #'swift-mode:test-current-line-beginning-of-defun
                              swift-file
                              current-line
                              error-buffer
                              test-parameter))
                     (count-assoc (assq status error-counts)))
                (setcdr count-assoc (1+ (cdr count-assoc)))))
            (forward-line)))))))

(defun swift-mode:parse-beginning-of-defun-test-file ()
  "Parse the current buffer as a test file and return its structure.

The result is a list of remarkable tokens in descendant order.  A remarkable
token is a list with the follwing elements:

1. Type; one of `beginning-of-defun', `end-of-defun', `beginning-of-sentence',
`end-of-sentence', `{', or `}'
2. Start position
3. End position
4. Nesting level at the start position
5. Nesting level at the end position

`beginning-of-defun', `end-of-defun', `beginning-of-sentence', and
`end-of-sentence' are represented as /*{*/, /*}*/, /*[*/, and /*]*/,
respectively, in the test file, and removed from the buffer.

`{' and `}' includes square brackets and parentheses."
  (save-excursion
    (goto-char (point-min))
    (let ((expected-positions
           (list (list 'beginning-of-defun (point) (point) 0 0)))
          (depth 0)
          (pattern (mapconcat #'regexp-quote
                              '("/*{*/" "/*}*/" "/*[*/" "/*]*/"
                                "{" "}" "[" "]" "(" ")"
                                "/*" "*/"
                                "//" "\n"
                                "\"\"\""
                                "\""
                                )
                              "\\|"))
          match-string
          match-beginning
          match-end)
      (while (search-forward-regexp pattern nil t)
        (setq match-string (match-string-no-properties 0))
        (setq match-beginning (match-beginning 0))
        (setq match-end (match-end 0))
        (cond
         ((equal match-string "/*{*/")
          (add-to-list 'expected-positions
                       (list 'beginning-of-defun
                             match-beginning match-beginning
                             depth depth))
          (replace-match ""))
         ((equal match-string "/*}*/")
          (add-to-list 'expected-positions
                       (list 'end-of-defun
                             match-beginning match-beginning
                             depth depth))
          (replace-match ""))
         ((equal match-string "/*[*/")
          (add-to-list 'expected-positions
                       (list 'beginning-of-sentence
                             match-beginning match-beginning
                             depth depth))
          (replace-match ""))
         ((equal match-string "/*]*/")
          (add-to-list 'expected-positions
                       (list 'end-of-sentence
                             match-beginning match-beginning
                             depth depth))
          (replace-match ""))
         ((and (member match-string '("{" "[" "(" "/*"))
               (not (swift-mode:chunk-after match-beginning)))
          (setq depth (1+ depth))
          (add-to-list 'expected-positions
                       (list '{ match-beginning match-end (1- depth) depth)))
         ((and (member match-string '("}" "]" ")" "*/"))
               (not (swift-mode:chunk-after match-end)))
          (setq depth (1- depth))
          (add-to-list 'expected-positions
                       (list '} match-beginning match-end (1+ depth) depth)))

         ((and (equal match-string "//")
               (not (swift-mode:chunk-after match-beginning)))
          (setq depth (1+ depth))
          (add-to-list 'expected-positions
                       (list '{ match-beginning match-end (1- depth) depth)))
         ((and (equal match-string "\n")
               (eq (swift-mode:chunk:type
                    (swift-mode:chunk-after match-beginning))
                   'single-line-comment))
          (if (looking-at "\\s *//")
              ;; Fuses with next line.
              (goto-char (match-end 0))
            (setq depth (1- depth))
            (add-to-list 'expected-positions
                         (list '} match-beginning match-end (1+ depth) depth))))
         ((and (equal match-string "\"\"\"")
               (not (eq (char-before match-beginning) ?\\))
               (not (swift-mode:chunk:comment-p
                     (swift-mode:chunk-after match-beginning))))
          (if (swift-mode:chunk:multiline-string-p
               (swift-mode:chunk-after match-end))
              (progn (setq depth (1+ depth))
                     (add-to-list
                      'expected-positions
                      (list '{ match-beginning match-end (1- depth) depth)))
            (setq depth (1- depth))
            (add-to-list
             'expected-positions
             (list '} match-beginning match-end (1+ depth) depth))))
         ((and (equal match-string "\"")
               (not (eq (char-before match-beginning) ?\\))
               (not (swift-mode:chunk:comment-p
                     (swift-mode:chunk-after match-beginning)))
               (not (swift-mode:chunk:multiline-string-p
                     (swift-mode:chunk-after match-beginning))))
          (if (swift-mode:chunk:single-line-string-p
               (swift-mode:chunk-after match-end))
              (progn (setq depth (1+ depth))
                     (add-to-list
                      'expected-positions
                      (list '{ match-beginning match-end (1- depth) depth)))
            (setq depth (1- depth))
            (add-to-list
             'expected-positions
             (list '} match-beginning match-end (1+ depth) depth))))))
      (goto-char (point-max))
      (add-to-list 'expected-positions
                   (list 'end-of-defun (point) (point) depth depth))
      expected-positions)))

(defun swift-mode:test-current-line-beginning-of-defun
    (swift-file
     current-line
     error-buffer
     expected-positions
     less-than-function
     beginning-of-thing-function
     boundary-symbols)
  "Run `beginning-of-defun' test for `swift-mode' on current line.

SWIFT-FILE is the filename of the current test case.
CURRENT-LINE is the current line number.
ERROR-BUFFER is the buffer to output errors.
EXPECTED-POSITIONS is a list of remarkable tokens
\(see `swift-mode:parse-beginning-of-defun-test-file').
LESS-THAN-FUNCTION is a function returns non-nil iff the firt argument is
before (or after for `end-of-defun' test) the second argument.
BEGINNING-OF-THING-FUNCTION is a function goes to the boundary, that is the
beginning of a defun or the end of the defun..
BOUNDARY-SYMBOLS is the type or the list of types of expected remarkable token,
like `beginning-of-defun' or `end-of-defun'"
  (when (symbolp boundary-symbols)
    (setq boundary-symbols (list boundary-symbols)))
  (forward-line 0)
  (let ((status 'ok)
        depth
        expected-positions-before-point
        expected-position
        actual-position)
    (while (eq status 'ok)
      (setq expected-positions-before-point
            (seq-drop-while
             (lambda (position)
               (funcall less-than-function (point) (nth 1 position)))
             expected-positions))
      (setq depth (or (nth 2 (car expected-positions-before-point)) 0))
      (setq expected-position
            (nth 1 (seq-find
                    (lambda (position)
                      (setq depth (min depth (nth 2 position)))
                      (and
                       (memq (nth 0 position) boundary-symbols)
                       (funcall less-than-function (nth 1 position) (point))
                       (<= (nth 2 position) depth)))
                    expected-positions-before-point
                    (list nil (point-min) nil))))
      (setq actual-position (save-excursion
                              (funcall beginning-of-thing-function)
                              (point)))
      (when (/= expected-position actual-position)
        (setq status 'error)
        (swift-mode:show-error
         error-buffer swift-file current-line
         "error"
         (concat
          (symbol-name (car boundary-symbols))
          ": at "
          (prin1-to-string (point))
          ", expected "
          (prin1-to-string expected-position)
          " but "
          (prin1-to-string actual-position))))
      (if (eolp)
          (setq status 'done)
        (forward-char)))
    (when (eq status 'done)
      (setq status 'ok))
    status))

(provide 'swift-mode-test-beginning-of-defun)

;;; swift-mode-test-beginning-of-defun.el ends here
