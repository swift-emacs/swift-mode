;;; swift-mode.el --- Major-mode for Apple's Swift programming language.

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <>
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

;; Major-mode for Apple's Swift programming language.

;;; Code:

(require 'dash)
(require 'rx)

;; Font lock.

(defvar swift-mode--declaration-keywords
  '("class" "deinit" "enum" "extension" "func" "import" "init" "let"
    "protocol" "static" "struct" "subscript" "typealias" "var"))

(defvar swift-mode--statment-keywords
  '("break" "case" "continue" "default" "do" "else" "fallthrough"
    "if" "in" "for" "return" "switch" "where" "while"))

(defvar swift-mode--expression-keywords
  '("as" "dynamicType" "is" "new" "super" "self" "Self" "Type"
    "__COLUMN__" "__FILE__" "__FUNCTION__" "__LINE__"))

(defvar swift-mode--contextual-keywords
  '("associativity" "didSet" "get" "infix" "inout" "left" "mutating" "none"
    "nonmutating" "operator" "override" "postfix" "precedence" "prefix" "right"
    "set" "unowned" "unowned(safe)" "unowned(unsafe)" "weak" "willSet"))

(defvar swift-mode--keywords
  (-flatten (list swift-mode--declaration-keywords
                  swift-mode--statment-keywords
                  swift-mode--expression-keywords
                  swift-mode--contextual-keywords))
  "Keywords used in the Swift language.")

(defvar swift-mode--font-lock-defaults
  (list
   (list

    ;; Keywords
    ;;
    ;; Swift allows reserved words to be used as identifiers when enclosed
    ;; with backticks, in which case they should be highlighted as
    ;; identifiers, not keywords.
    (cons
     (rx-to-string `(and (or bol (not (any "`"))) bow
                         (group (or ,@swift-mode--keywords))
                         eow)
                   t)
     1)

    ;; Types
    ;;
    ;; Any token beginning with an uppercase character is highlighted as a
    ;; type.
    (cons (rx bow upper (+ word) eow)
          font-lock-type-face)

    ;; Function names
    ;;
    ;; Any string beginning after the `func' keyword is highlighted as a
    ;; function name.
    (cons (rx bow "func" eow (+ space) (group bow (+ word) eow))
          (list 1 font-lock-function-name-face)))
   )
  "Font lock values for `swift-mode'.")

;; Mode definition.

(defvar swift-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in `swift-mode'.")

(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for Apple's Swift programming language.

\\<swift-mode-map>"
  :group 'swift
  (setq-local font-lock-defaults swift-mode--font-lock-defaults))

(provide 'swift-mode)

;;; swift-mode.el ends here
