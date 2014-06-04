;;; swift-indent.el --- Indentation commands for Swift mode.

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

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

;; Indentation commands for Swift mode. It's based on the code for `rust-mode',
;; which is very clean.

;;; Code:

(defgroup swift-indent nil
  "Configuration for swift-mode indentation behaviour."
  :group 'languages
  :prefix "swift-indent-")

(defcustom swift-indent-offset 4
  "Defines the indentation offset for Swift code."
  :group 'swift-indent
  :type 'integer)

(defun swift-indent--paren-level ()
  "Return the paren level at point."
  (nth 0 (syntax-ppss)))

(defun swift-indent--in-str-or-cmnt ()
  "Non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss)))

(defun swift-indent--back-to-start-of-level ()
  "Move backwards up to the start of the current indentation level."
  (let ((current-level (swift-indent--paren-level)))
    (back-to-indentation)
    (while (> (swift-indent--paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))))

(defun swift-indent--rewind-past-str-cmnt ()
  (goto-char (nth 8 (syntax-ppss))))

(defun swift-indent--rewind-irrelevant ()
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (looking-back "\\*/") (backward-char))
    (if (swift-indent--in-str-or-cmnt)
        (swift-indent--rewind-past-str-cmnt))
    (if (/= starting (point))
        (swift-indent--rewind-irrelevant))))

(defun swift-indent--align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line.
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
        (forward-word 1)
        (backward-word 1))
      (current-column))))

(defun swift-indent--rewind-to-beginning-of-current-level-expr ()
  (let ((current-level (swift-indent--paren-level)))
    (back-to-indentation)
    (while (> (swift-indent--paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))))

(defun swift-indent--calculate-indentation ()
  "Calculate the indentation column to use for `swift-indent-line'.
Returns the column number as an integer."
  (save-excursion
    (back-to-indentation)
    ;; Point is now at beginning of line.
    (let* ((level (swift-indent--paren-level))
           ;; Our "baseline" is one level out from the indentation of the
           ;; expression containing the innermost enclosing opening bracket.
           ;; That way if we are within a block that has a different indentation
           ;; than this mode would give it, we still indent the inside of it
           ;; correctly relative to the outside.
           (baseline
            (if (zerop level)
                0
              (save-excursion
                (backward-up-list)
                (swift-indent--back-to-start-of-level)
                (+ (current-column) swift-indent-offset)))))
      (cond
       ;; A function return type is indented to the corresponding function arguments
       ((looking-at "->")
        (save-excursion
          (backward-list)
          (or (swift-indent--align-to-expr-after-brace)
              (+ baseline swift-indent-offset))))

       ;; A closing brace is 1 level unindented
       ((looking-at "}") (- baseline swift-indent-offset))

       ;; Doc comments in /** style with leading * indent to line up the *s
       ((and (nth 4 (syntax-ppss)) (looking-at "*"))
        (+ 1 baseline))

       ;; If we're in any other token-tree / sexp, then:
       (t
        (or
         ;; If we are inside a pair of braces, with something after the
         ;; open brace on the same line and ending with a comma, treat
         ;; it as fields and align them.
         (when (> level 0)
           (save-excursion
             (swift-indent--rewind-irrelevant)
             (backward-up-list)
             ;; Point is now at the beginning of the containing set of braces
             (swift-indent--align-to-expr-after-brace)))

         (progn
           (back-to-indentation)
           ;; Point is now at the beginning of the current line
           (cond
            ;; If this line begins with "else" or "{", stay on the
            ;; baseline as well (we are continuing an expression,
            ;; but the "else" or "{" should align with the beginning
            ;; of the expression it's in.)
            ((looking-at (rx (or (and bow "else" eow) "{")))
             baseline)
            ;; Cases are indented to the same level as the enclosing switch statement.
            ((looking-at (rx bow (or "case" "default") eow))
             (- baseline swift-indent-offset))

            ;; If we are at the first line, no indentation is needed, so stay at baseline.
            ((save-excursion
               (swift-indent--rewind-irrelevant)
               ;; Point is now at the end of the previous line
               (= 1 (line-number-at-pos (point))))
             baseline)

            ((save-excursion
               (swift-indent--rewind-irrelevant)
               ;; Point is now at the end of the previous line
               ;; If the previous line ends with any of these:
               ;;     { ? : ( , ; [ }
               ;; then we are at the beginning of an expression, so stay on the baseline.
               (looking-back "[(,:;?[{}]\\|[^|]|"))
             baseline)

            (t
             baseline)))))))))

(defun swift-indent-line ()
  "Indent the current line.  Also see `swift-indent-offset'."
  (interactive "*")
  (let ((indent (swift-indent--calculate-indentation)))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent)
      (save-excursion
        (indent-line-to indent)))))

(provide 'swift-indent)

;;; swift-indent.el ends here
