;;; swift-mode-test-indent.el --- Test for swift-mode: indentation  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 taku0

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

;; Test for swift-mode: indentation.
;; Execute swift-mode:run-test:indent interactively or in batch mode.

;;; Code:

(require 'swift-mode)
(require 'swift-mode-indent)

(defun swift-mode:run-test:indent
    (&optional error-buffer error-counts progress-reporter)
  "Run indentation test for `swift-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors. Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)

  (if (not swift-mode:test:running)
      (swift-mode:run-test '(swift-mode:run-test:indent))
    (let ((current-line 0))
      (setq default-directory
            (concat (file-name-as-directory swift-mode:test:basedir)
                    (file-name-as-directory "swift-files")
                    "indent"))

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
            (forward-line)))))))

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
        (if known-bug "(known bug) " "")
        "indent: expected "
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

(provide 'swift-mode-test-indent)

;;; swift-mode-test-indent.el ends here
