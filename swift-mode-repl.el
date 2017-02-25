;;; swift-mode-repl.el --- Run Apple's Swift processes in Emacs buffers -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;
;; Version: 2.2.4
;; Package-Requires: ((emacs "24.4"))
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

;; Run Apple's Swift processes in Emacs buffers.

;;; Code:

(require 'comint)

(defcustom swift-mode:repl-executable
  "xcrun swift"
  "Path to the Swift CLI."
  :type 'string
  :group 'swift
  :safe 'stringp)

(defvar swift-mode:repl-buffer nil
  "Stores the name of the current swift REPL buffer, or nil.")

;;;###autoload
(defun swift-mode:run-repl (cmd &optional dont-switch)
  "Run a Swift REPL process.
It input and output via buffer `*CMD*' where CMD is replaced with the CMD given.
If there is a process already running in `*CMD*', switch to that buffer.
With argument CMD allows you to edit the command line (default is value
of `swift-mode:repl-executable').  This function updates the buffer local
variable `swift-mode:repl-executable' with the given CMD, so it will be used
as the default value for the next invocatoin in the current buffer.
With DONT-SWITCH cursor will stay in current buffer.
Runs the hook `swift-repl-mode-hook' \(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive
   (list (if current-prefix-arg
             (read-string "Run swift REPL: " swift-mode:repl-executable)
           swift-mode:repl-executable)))
  (let ((original-buffer (current-buffer))
        (buffer (get-buffer-create (concat "*" cmd "*"))))
    (unless dont-switch
      (pop-to-buffer buffer))
    (unless (comint-check-proc (concat "*" cmd "*"))
      (save-excursion
        (let ((cmdlist (split-string cmd)))
          (apply 'make-comint-in-buffer
                 cmd buffer (car cmdlist) nil (cdr cmdlist))
          (swift-repl-mode))))
    (with-current-buffer original-buffer
      (setq-local swift-mode:repl-executable cmd)
      (setq-local swift-mode:repl-buffer (concat "*" cmd "*"))
      (setq-default swift-mode:repl-buffer swift-mode:repl-buffer))))

;;;###autoload
(defalias 'run-swift 'swift-mode:run-repl)

;;;###autoload
(defun swift-mode:send-region (start end)
  "Send the current region to the inferior swift process.
START and END define region within current buffer"
  (interactive "r")
  (swift-mode:run-repl swift-mode:repl-executable t)
  (comint-send-region swift-mode:repl-buffer start end)
  (comint-send-string swift-mode:repl-buffer "\n"))

;;;###autoload
(defun swift-mode:send-buffer ()
  "Send the buffer to the Swift REPL process."
  (interactive)
  (swift-mode:send-region (point-min) (point-max)))

(define-derived-mode swift-repl-mode comint-mode "Swift REPL"
  "Major mode for interacting with Swift REPL.

A REPL can be fired up with M-x swift-mode:run-repl or M-x run-swift.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
swift-repl-mode-hook (in that order).

You can send text to the REPL process from other buffers containing source.
    swift-mode:send-region sends the current region to the REPL process,
    swift-mode:send-buffer sends the current buffer to the REPL process.")

(provide 'swift-mode-repl)

;;; swift-mode-repl.el ends here
