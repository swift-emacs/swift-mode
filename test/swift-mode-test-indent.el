;;; swift-mode-test-indent.el --- Test for swift-mode: indentation  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017 taku0

;; Authors: taku0 (http://github.com/taku0)
;;
;; Version: 3.0.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
;; Keywords: languages swift
;; URL: https://github.com/swift-emacs/swift-mode

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

;; Test for swift-mode: indentation.
;; Execute swift-mode:run-test:indent interactively or in batch mode.

;;; Code:

(require 'swift-mode)
(require 'swift-mode-indent)

(defvar swift-mode:test:basedir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun swift-mode:setup-error-buffer ()
  "Initialize and switch to the error buffer.

Return the error-buffer"
  (pop-to-buffer (get-buffer-create "*swift-mode-test-indent*"))
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (current-buffer))

(defun swift-mode:run-test:indent ()
  "Run indentation test for `swift-mode'."
  (interactive)
  (let ((error-buffer
         (if noninteractive nil (swift-mode:setup-error-buffer)))
        (current-line 0)
        (error-counts (list
                       (cons 'error 0)
                       (cons 'warning 0)
                       (cons 'info 0)
                       (cons 'ok 0)))
        (progress-reporter (unless noninteractive
                             (make-progress-reporter "Running tests..."))))
    (setq default-directory
          (concat (file-name-as-directory swift-mode:test:basedir)
                  "swift-files"))

    (dolist (swift-file (file-expand-wildcards "*.swift"))
      (redisplay)
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert-file-contents-literally swift-file)
        (swift-mode)
        (setq current-line 0)
        (while (not (eobp))
          (when (not noninteractive)
            (progress-reporter-update progress-reporter))
          (setq current-line (1+ current-line))
          (cond
           ((looking-at ".*//.*swift-mode:test:keep-indent")
            nil)

           ((= (line-beginning-position) (line-end-position))
            ;; Empty line
            nil)

           (t
            (when (looking-at ".*//.*swift-mode:test:eval\\(.*\\)")
              (eval-region (match-beginning 1) (match-end 1)))
            (let*
                ((status (swift-mode:test-current-line-indent
                          swift-file current-line error-buffer))
                 (count-assoc (assq status error-counts)))
              (setcdr count-assoc (1+ (cdr count-assoc))))))
          (forward-line))))

    (when (not noninteractive)
      (progress-reporter-done progress-reporter))

    (swift-mode:print-message
     error-buffer
     (concat
      "Errors: " (prin1-to-string (assoc-default 'error error-counts)) "\n"
      "Warning: " (prin1-to-string (assoc-default 'warning error-counts)) "\n"
      "Info: " (prin1-to-string (assoc-default 'info error-counts)) "\n"
      "OK: " (prin1-to-string (assoc-default 'ok error-counts)) "\n"))

    (if noninteractive
        (kill-emacs (min 63 (assoc-default 'error error-counts)))
      (compilation-mode))))

(defun swift-mode:test-current-line-indent
    (swift-file current-line error-buffer)
  "Run indentation test for swift-mode on current line.

SWIFT-FILE is the filename of the current test case.
CURRENT-LINE is the current line number.
ERROR-BUFFER is the buffer to output errors."
  (back-to-indentation)
  (let ((original-indent (current-column))
        computed-indent
        (known-bug (looking-at ".*//.*swift-mode:test:known-bug"))
        (status 'ok))
    (delete-horizontal-space)
    (when (= original-indent 0)
      (indent-line-to 1))

    (swift-mode:indent-line)
    (back-to-indentation)
    (setq computed-indent (current-column))
    (indent-line-to original-indent)

    (when (/= original-indent computed-indent)
      (setq status (if known-bug 'warning 'error))

      (swift-mode:show-error
       error-buffer swift-file current-line
       (if known-bug "warning" "error")
       (concat
        (if known-bug "(knwon bug) " "")
        "expected "
        (prin1-to-string original-indent)
        " but "
        (prin1-to-string computed-indent))))

    (when (and (= original-indent computed-indent) known-bug)
      (setq status 'info)
      (swift-mode:show-error
       error-buffer swift-file current-line
       "info"
       "known-bug is fixed somehow"))

    status))

(defun swift-mode:show-error (error-buffer file line level message)
  "Show an error message to the ERROR-BUFFER or stdout.

If the Emacs is in the batch mode, the message is printed to the stdout.
Otherwise, the message is appended to the ERROR-BUFFER.

FILE is the filename of the test case.
LINE is the line number of the error.
LEVEL is the error level (e.g. error, warning).
MESSAGE is the error message."
  (let ((formatted
         (concat
          "swift-mode-test:"
          file
          ":"
          (prin1-to-string line)
          ": "
          level
          ": "
          message
          "\n")))
    (swift-mode:print-message error-buffer formatted)))

(defun swift-mode:print-message (error-buffer message)
  "Print a message to the ERROR-BUFFER or stdout.

If the Emacs is in the batch mode, MESSAGE is printed to the stdout.
Otherwise, MESSAGE is appended to the ERROR-BUFFER."
  (if noninteractive
      (princ message)
    (with-current-buffer error-buffer
      (goto-char (point-max))
      (insert-and-inherit message))))

(provide 'swift-mode-test-indent)

;;; swift-mode-test-indent.el ends here
