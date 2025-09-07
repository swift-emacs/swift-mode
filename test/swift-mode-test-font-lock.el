;;; swift-mode-test-font-lock.el --- Test for swift-mode: font-lock  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Daniel Martín

;; Author: Daniel Martín (http://github.com/taku0)

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

;; Test for swift-mode: font-lock.
;; Execute swift-mode:run-test:font-lock interactively or in batch mode.

;;; Code:

(require 'swift-mode)
(require 'swift-mode-test)
(require 'swift-mode-font-lock)

(defun swift-mode:run-test:font-lock
    (&optional error-buffer error-counts progress-reporter)
  "Run font-lock test for `swift-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors.  Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)
  (if (not swift-mode:test:running)
      (swift-mode:run-test '(swift-mode:run-test:font-lock))
    (setq default-directory
          (concat (file-name-as-directory swift-mode:test:basedir)
                  (file-name-as-directory "swift-files")
                  "font-lock"))
    (dolist (swift-file (file-expand-wildcards "*.swift"))
      (redisplay)
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert-file-contents-literally swift-file)
        (swift-mode)
        (let ((tests (swift-mode:parse-font-lock-test))
              status
              count-assoc)
          (funcall (if (fboundp 'font-lock-ensure)
                       #'font-lock-ensure
                     #'font-lock-fontify-buffer))
          (dolist (test tests)
            (when (not noninteractive)
              (progress-reporter-update progress-reporter))
            (setq status (swift-mode:test-font-lock-1
                          swift-file
                          (nth 0 test)
                          (nth 1 test)
                          (nth 2 test)
                          error-buffer))
            (setq count-assoc (assq status error-counts))
            (setcdr count-assoc (1+ (cdr count-assoc)))))))))

(defun swift-mode:parse-font-lock-test ()
  (save-excursion
    (goto-char (point-min))
    (let ((tests nil)
          (current-line 0))
      (while (progn
               (setq current-line (1+ current-line))
               (when (looking-at
                      "\\(.*\\)[\s\t]+//[\s\t]+EXPECTED:[\s\t]+\\(.*\\)$")
                 (push (list (point)
                             current-line
                             (read
                              (concat
                               "("
                               (match-string-no-properties 2)
                               ")")))
                       tests)
                 (delete-region (match-end 1) (line-end-position)))
               (eq 0 (forward-line)))
        t)
      (reverse tests))))

(defun swift-mode:test-font-lock-1
    (swift-file pos current-line expected error-buffer)
  "Compute the font-lock properties applied by Swift mode on the line at POS.

SWIFT-FILE is the filename of the current test case.
CURRENT-LINE is the current line number.
EXPECTED is expected faces computed by `swift-mode:get-faces-of-current-line'.
ERROR-BUFFER is the buffer to output errors."
  (save-excursion
    (goto-char pos)
    (let ((actual nil))
      (dolist (cons (swift-mode:get-faces-of-current-line))
        (push (car cons) actual)
        (push (cdr cons) actual))
      (setq actual (reverse actual))
      (if (equal expected actual)
          'ok
        (swift-mode:show-error
         error-buffer swift-file current-line
         "error"
         (format "font-lock: expected %S but %S" expected actual))
        'error))))

(defun swift-mode:get-faces-of-current-line ()
  (save-excursion
    (let ((faces nil))
      (beginning-of-line)
      (while (not (eolp))
        (let ((face (get-text-property (point) 'face))
              (start (point))
              (end (progn
                     (goto-char (next-single-property-change
                                 (point)
                                 'face
                                 nil
                                 (line-end-position)))
                     (point))))
          (when face
            (push (cons (buffer-substring-no-properties start end)
                        face)
                  faces))))
      (reverse faces))))

(defun swift-mode:add-expected ()
  (when (fboundp 'string-replace)
    (save-excursion
      (let ((faces (swift-mode:get-faces-of-current-line)))
        (end-of-line)
        (insert " // EXPECTED:")
        (dolist (tuple faces)
          (insert
           (format " %s %s"
                   (string-replace "*" "\\*" (prin1-to-string (car tuple)))
                   (cdr tuple))))))))

;; (progn
;;   (goto-char (point-max))
;;   (while (search-backward " // ★" nil t)
;;     (replace-match "")
;;     (swift-mode:add-expected)))

(provide 'swift-mode-test-font-lock)

;;; swift-mode-test-font-lock.el ends here
