;;; font-lock-tests.el --- Tests for font-lock behaviours in swift-mode.  -*- lexical-binding: t; -*-

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

;; Tests for font-lock behaviours in swift-mode.
;;
;; Uses puppet-mode tests as a reference:
;; https://github.com/lunaryorn/puppet-mode/blob/master/test/puppet-mode-test.el

;;; Code:


(require 'swift-mode)
(require 'ert)
(require 'cl-lib)
(require 's nil t)
(require 'dash nil t)

;;; Test utilities

(defun swift-test--moustache-substring (str)
  "Propertise STR and extract the portion between moustaches.
Returns the substring with properties."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (start end)
      ;; Remove open-moustache chars and save position.
      (search-forward "{{")
      (delete-char -2)
      (setq start (point))
      ;; Remove end-moustache chars and save position.
      (search-forward-regexp (rx (>= 2 "}")))
      (delete-char -2)
      (setq end (point))
      ;; Propertise buffer.
      (swift-mode)
      (font-lock-fontify-buffer)
      (buffer-substring start end))))

(defun swift-test--whole-string-has-face? (str face)
  "Non-nil if all of STR is propertised with FACE."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((points
           ;; Walk over the string and get the faces at each position.
           (cl-loop while (not (eobp))
                    collecting (get-text-property (point) 'face)
                    do (forward-char 1))))

      (--all? (cond ((null it)  (null face))
                    ((listp it) (member face it))
                    (t          (equal face it)))
              points))))

(defmacro check-face (description face str)
  "Declare an ert test for font-lock behaviour.
The test will check that a portion of the buffer is propertised with the
given face.

DESCRIPTION is a symbol describing the test.

FACE is the face name as an unquoted symbol.  It can also be nil, which
means the string should not have any face.

STR is the string to test for face propertisation.  The portion of the
string to be checked is surrounded with moustaches ('{{' and '}}'), as
below:

   let {{foo}} = y

The whole substring between moustaches must be propertised with FACE or the
test will fail."
  (declare (indent 2))
  (cl-assert (symbolp description))
  (cl-assert (or (facep face) (null face)))
  (cl-assert (stringp str))
  (cl-assert (s-contains? "{{" str))
  (cl-assert (s-contains? "}}" str))
  (cl-assert (<= 1 (length (swift-test--moustache-substring str))))

  (let ((test-name (intern (format "font-lock/%s" description))))
    `(ert-deftest ,test-name ()
       (let ((substr (swift-test--moustache-substring ,str)))
         (should (swift-test--whole-string-has-face? substr ,face))))))

;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "check-face" eow))
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "check-face") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))

;;; Tests

(check-face let/has-keyword-face/1 font-lock-keyword-face "{{let}}")
(check-face let/has-keyword-face/2 font-lock-keyword-face " {{let}}")
(check-face let/has-keyword-face/3 font-lock-keyword-face "{{let}} x = y")
(check-face let-bind/has-variable-face/1 font-lock-variable-name-face "let {{x}} = y")
(check-face let-bind/has-variable-face/2 font-lock-variable-name-face "let {{foo}} = y")
(check-face let-bind/has-variable-face/3 font-lock-variable-name-face "let {{x}}: T = y")
(check-face let-bind-type-ann/has-type-face/1 font-lock-type-face "let x: {{T}} = y")
(check-face let-bind-type-ann/has-type-face/2 font-lock-type-face "let x: {{Type}} = y")

(check-face var/has-keyword-face/1 font-lock-keyword-face "{{var}}")
(check-face var/has-keyword-face/2 font-lock-keyword-face " {{var}}")
(check-face var/has-keyword-face/3 font-lock-keyword-face "{{var}} x = y")
(check-face var-bind/has-variable-face/1 font-lock-variable-name-face "var {{x}} = y")
(check-face var-bind/has-variable-face/2 font-lock-variable-name-face "var {{foo}} = y")
(check-face var-bind/has-variable-face/3 font-lock-variable-name-face "var {{x}}: T = y")
(check-face var-bind-type-ann/has-type-face/1 font-lock-type-face "var x: {{T}} = y")
(check-face var-bind-type-ann/has-type-face/2 font-lock-type-face "var x: {{Type}} = y")

(check-face func/has-keyword-face/1 font-lock-keyword-face "{{func}}")
(check-face func/has-keyword-face/2 font-lock-keyword-face "{{func}} x() {})")
(check-face func-name/has-function-name-face/1 font-lock-function-name-face "func {{x}}")
(check-face func-name/has-function-name-face/2 font-lock-function-name-face "func {{foo}}")
(check-face func-name/has-function-name-face/3 font-lock-function-name-face "func {{foo}}()")
(check-face func-name/has-function-name-face/4 font-lock-function-name-face "func {{foo}}<T>")
(check-face func-name/has-function-name-face/6 font-lock-function-name-face
  "func {{foo}}<T>(param: T) -> U {}")

(check-face func-return-type/has-type-face/1 font-lock-type-face "func foo() -> {{U}} {}")
(check-face func-return-type/arrow-has-default-face/1 nil "func foo() {{->}} U {}")

(check-face enum/has-keyword-face/1 font-lock-keyword-face "{{enum}} T")
(check-face enum/has-keyword-face/2 font-lock-keyword-face "{{enum}} T")
(check-face enum/type-has-type-face/1 font-lock-type-face "enum {{T}}")
(check-face enum/type-has-type-face/2 font-lock-type-face "enum {{Type}}")
(check-face enum/type-has-type-face/3 font-lock-type-face "enum {{T}} {}")
(check-face enum/type-has-type-face/4 font-lock-type-face "enum {{T}} {\n}")
(check-face enum/generic-parameter/has-type-face/1 font-lock-type-face "enum N<{{T}}>")
(check-face enum/generic-parameter/has-type-face/2 font-lock-type-face "enum Name<{{T}}> {}")
(check-face enum/generic-parameter/has-type-face/3 font-lock-type-face "enum Name <{{T}}> {}")
(check-face enum/generic-parameter/brackets-have-default-face/1 nil "enum N{{<}}T>")
(check-face enum/generic-parameter/brackets-have-default-face/2 nil "enum N<T{{>}}")

(check-face class/has-keyword-face/1 font-lock-keyword-face "{{class}} T")
(check-face class/has-keyword-face/2 font-lock-keyword-face "{{class}} T")
(check-face class/type-has-type-face/1 font-lock-type-face "class {{T}}")
(check-face class/type-has-type-face/2 font-lock-type-face "class {{Type}}")
(check-face class/type-has-type-face/3 font-lock-type-face "class {{T}} {}")
(check-face class/type-has-type-face/4 font-lock-type-face "class {{T}} {\n}")
(check-face class/type-has-type-face/5 font-lock-type-face "class {{T}}: Base")

(check-face class/body-brackets-have-default-face nil "class T {{{}}}")

(check-face class/generic-parameter/has-type-face/1 font-lock-type-face "class N<{{T}}>")
(check-face class/generic-parameter/has-type-face/2 font-lock-type-face "class Name<{{T}}> {}")
(check-face class/generic-parameter/has-type-face/3 font-lock-type-face "class Name <{{T}}> {}")
(check-face class/generic-parameter/brackets-have-default-face/1 nil "class N{{<}}T>")
(check-face class/generic-parameter/brackets-have-default-face/2 nil "class N<T{{>}}")

(check-face class/base-type-has-type-face/1 font-lock-type-face "class T:{{Base}}")
(check-face class/base-type-has-type-face/2 font-lock-type-face "class T: {{Base}}")
(check-face class/base-type-has-type-face/3 font-lock-type-face "class T : {{Base}}")
(check-face class/base-type-has-type-face/4 font-lock-type-face "class T<U> : {{Base}}")
(check-face class/base-type-colon-has-default-face/1 nil "class T {{:}} Base")

(provide 'font-lock-tests)

;;; font-lock-tests.el ends here
