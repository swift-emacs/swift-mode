;;; swift-mode-test.el --- Test for swift-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019 taku0

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

;; Tests for swift-mode.
;; Execute swift-mode:run-test interactively or in batch mode.

;;; Code:

(require 'swift-mode-test-indent)
(require 'swift-mode-test-beginning-of-defun)
(require 'swift-mode-test-imenu)
(require 'swift-mode-test-font-lock)
(require 'swift-mode-test-fill)

(defvar swift-mode:test:basedir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar swift-mode:test:running nil)

(defun swift-mode:setup-error-buffer ()
  "Initialize and switch to the error buffer.

Return the error-buffer"
  (pop-to-buffer (get-buffer-create "*swift-mode-test*"))
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (current-buffer))

(defvar swift-mode:tests
  '(swift-mode:run-test:indent
    swift-mode:run-test:beginning-of-defun
    swift-mode:run-test:imenu
    swift-mode:run-test:font-lock
    swift-mode:run-test:fill))

(defun swift-mode:run-test (&optional tests)
  "Run TESTS for `swift-mode'."
  (interactive)

  (setq tests (or tests swift-mode:tests))

  (let ((error-buffer
         (if noninteractive nil (swift-mode:setup-error-buffer)))
        (error-counts (list
                       (cons 'error 0)
                       (cons 'warning 0)
                       (cons 'info 0)
                       (cons 'ok 0)))
        (progress-reporter (unless noninteractive
                             (make-progress-reporter "Running tests..."))))

    (setq swift-mode:test:running t)

    (unwind-protect
        (dolist (test tests)
          (funcall test error-buffer error-counts progress-reporter))
      (setq swift-mode:test:running nil))

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

(provide 'swift-mode-test)

;;; swift-mode-test.el ends here
