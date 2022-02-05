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
(require 'swift-mode-fill)

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
    (let ((current-line 0))
      (setq default-directory
            (concat (file-name-as-directory swift-mode:test:basedir)
                    (file-name-as-directory "swift-files")
                    "fill"))

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
             ((= (line-beginning-position) (line-end-position))
              ;; Empty line
              nil)

             ((looking-at ".*//.*swift-mode:test:eval\\(.*\\)")
              (eval-region (match-beginning 1) (match-end 1)))

             ((looking-at ".*//.*swift-mode:test:case-begin")
              (progn
                ;; Marker comments must have a blank line separating them from
                ;; the test input or they will interfere with filling.
                (forward-line 2)
                (let*
                    ((status (swift-mode:test-current-case-fill
                              swift-file current-line error-buffer))
                     (count-assoc (assq status error-counts)))
                  (setcdr count-assoc (1+ (cdr count-assoc))))))

             (t (swift-mode:show-error
                 error-buffer swift-file current-line
                 "warning" "Unexpected test case input")))
            (forward-line)))))))

(defun swift-mode:test-current-case-fill (swift-file current-line error-buffer)
  "Run the current fill test case.

This applies `fill-paragraph' to the input string and compares the result
to the expected value.

SWIFT-FILE is the filename of the current test case.
CURRENT-LINE is the current line number.
ERROR-BUFFER is the buffer to output errors."
  (let (expected
        computed
        (status 'ok))
    (setq expected (swift-mode:test-fill:capture-expected))
    ;; Marker comments must have a blank line separating them from
    ;; the test input or they will interfere with filling.
    (forward-line 2)
    (setq computed (swift-mode:test-fill:perform-fill))
    (forward-line)

    (when (not (string= expected computed))
      (setq status 'error)

      (swift-mode:show-error
       error-buffer swift-file current-line
       "error"
       (concat "Fill region failure\n"
               "Expected: ```\n" expected "```\n\n"
               "Actual: ```\n" computed "```")))
    status))

(defsubst swift-mode:test-fill:capture-expected ()
  "Collect the expected result for this test case into a string."
  (let ((start (point))
        (end (point)))
    (while (not (looking-at ".*//.*swift-mode:test:case-input-begin"))
      (setq end (point))
      (forward-line))
    (buffer-substring start end)))

(defsubst swift-mode:test-fill:perform-fill ()
  "Run `fill-paragraph' on the case's input, returning the resulting string."
  (let ((start (point))
        end)
    (while (not (looking-at ".*//.*swift-mode:test:case-end"))
      (setq end (point))
      (forward-line))

    ;; Filling should work correctly from any place inside the contents
    (goto-char start)
    (forward-char (random (- (1- end) start)))
    (fill-paragraph)

    ;; Newline characters may have been added; find new end
    (beginning-of-line)
    (while (not (looking-at ".*//.*swift-mode:test:case-end"))
      (setq end (point))
      (forward-line))
    (buffer-substring start end)))

(provide 'swift-mode-test-fill)

;;; swift-mode-test-fill.el ends here
