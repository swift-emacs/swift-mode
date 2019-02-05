;;; swift-mode-imenu.el --- Major-mode for Apple's Swift programming language, , Imenu -*- lexical-binding: t -*-

;; Copyright (C) 2019 taku0

;; Authors: taku0 (http://github.com/taku0)
;;
;; Version: 7.1.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
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

;; List declarations for Imenu

;;; Code:

(require 'swift-mode-lexer)
(require 'seq)

;;;###autoload
(defgroup swift-mode:imenu nil
  "Imenu."
  :group 'swift)

(defcustom swift-mode:imenu-style
  'nested
  "Style of Imenu hierarchy.

Values:

- `nested': Class and its members are organized as trees.
- `flat': Organized into a flat list of fully qualified names."
  :type '(choice (const :tag "Nested" nested)
                 (const :tag "Flat" flat))
  :group 'swift-mode:imenu
  :safe 'symbolp)

(defun swift-mode:declaration (type name-token children)
  "Construct and return a declaration.

TYPE is the type of the declaration such as `class' or `struct'.
NAME-TOKEN is the name token of the declaration.  For declarations like `init',
it is the keyword token itself.
CHILDREN is the child declarations if exists."
  (list type name-token children))

(defun swift-mode:declaration:type (declaration)
  "Return the type of DECLARATION."
  (nth 0 declaration))

(defun swift-mode:declaration:name-token (declaration)
  "Return the name token of DECLARATION."
  (nth 1 declaration))

(defun swift-mode:declaration:children (declaration)
  "Return the children of DECLARATION."
  (nth 2 declaration))


(defun swift-mode:imenu-create-index (&optional style)
  "Create an index alist of the current buffer for Imenu.

STYLE is either `nested' or `flat', defaults to `nested'.
If it is `nested', class and its members are organized as trees.
If it is `flat', declarations are organized into a flat list of fully qualified
names."
  (unless style (setq style swift-mode:imenu-style))
  (save-excursion
    (goto-char (point-min))

    (let ((declarations '())
          (customization-item (list
                               "*Customize*"
                               0
                               (lambda (_name _position)
                                 (customize-group 'swift-mode:imenu)))))
      (while (not (eq (swift-mode:token:type
                       (save-excursion (swift-mode:forward-token)))
                      'outside-of-buffer))
        (setq declarations
              (append (swift-mode:scan-declarations) declarations)))
      (append (if (eq style 'flat)
                  (swift-mode:format-for-imenu:flat (nreverse declarations))
                (swift-mode:format-for-imenu:nested (nreverse declarations)))
              (list customization-item)))))

(defun swift-mode:scan-declarations ()
  "Scan declarations from current point.

Return found declarations in reverse order."
  (let (next-token
        next-type
        next-text
        name-token
        last-class-token
        (done nil)
        (declarations '()))
    (while (not done)
      (setq next-token
            (swift-mode:forward-token-or-list-except-curly-bracket))
      (setq next-type (swift-mode:token:type next-token))
      (setq next-text (swift-mode:token:text next-token))

      (cond
       ((equal next-text "import")
        ;; Skips an import kind, for example, "class" token below:
        ;;
        ;; import class Foo.Bar
        (swift-mode:forward-token-or-list-except-curly-bracket))

       ((equal next-text "class")
        ;; "class" token may be either a class declaration keyword or a
        ;; modifier:
        ;;
        ;; // Nested class named "final"
        ;; class Foo { class final {} }
        ;;
        ;; // Non-overridable class method named "foo"
        ;; class Foo { class final func foo() {} }
        ;;
        ;; So delays until "{" token.
        (setq last-class-token next-token))

       ((memq next-type '(\; implicit-\; { } outside-of-buffer))
        (when (memq next-type '(} outside-of-buffer))
          (setq done t))
        (cond
         ;; Having pending "class" token
         (last-class-token
          (save-excursion
            (goto-char (swift-mode:token:end last-class-token))
            (setq name-token (swift-mode:forward-token)))
          (setq last-class-token nil)
          (when (eq (swift-mode:token:type name-token) 'identifier)
            (push
             (swift-mode:declaration
              'class
              name-token
              (when (eq (swift-mode:token:type next-token) '{)
                (nreverse (swift-mode:scan-declarations))))
             declarations)))

         ;; Closure or other unknown block
         ((eq next-type '{)
          (goto-char (swift-mode:token:start next-token))
          (swift-mode:forward-token-or-list))

         ;; Ignores the token otherwise.
         ))

       ((member next-text '("struct" "protocol" "extension" "enum"))
        (setq last-class-token nil)
        (let ((declaration
               (swift-mode:scan-declarations:handle-struct-like next-token)))
          (when declaration
            (push declaration declarations))))

       ((equal next-text "case")
        (setq last-class-token nil)
        (let ((case-declarations
               (swift-mode:scan-declarations:handle-case-or-variable 'case)))
          (setq declarations (append case-declarations declarations))))

       ((member next-text '("typealias" "associatedtype"))
        (setq last-class-token nil)
        (setq name-token
              (swift-mode:forward-token-or-list-except-curly-bracket))
        (when (eq (swift-mode:token:type name-token) 'identifier)
          (push
           (swift-mode:declaration (intern next-text) name-token nil)
           declarations)))

       ((member next-text '("func" "init" "subscript"))
        (setq last-class-token nil)
        (unless (equal next-text "func")
          (goto-char (swift-mode:token:start next-token)))
        (let ((declaration-type (intern next-text))
              (names (swift-mode:scan-function-name-and-parameter-names)))
          (when names
            (setq name-token (car names))
            (push
             (swift-mode:declaration
              declaration-type
              (swift-mode:token
               (swift-mode:token:type name-token)
               (concat (swift-mode:token:text name-token)
                       "("
                       (mapconcat
                        (lambda (token)
                          (concat (swift-mode:token:text token) ":"))
                        (cdr names)
                        "")
                       ")")
               (swift-mode:token:start name-token)
               (swift-mode:token:end name-token))
              nil)
             declarations))))

       ((equal next-text "deinit")
        (setq last-class-token nil)
        (push (swift-mode:declaration 'deinit next-token nil) declarations))

       ((member next-text '("let" "var"))
        (setq last-class-token nil)
        (let ((variable-declarations
               (swift-mode:scan-declarations:handle-case-or-variable
                (intern next-text))))
          (setq declarations (append variable-declarations declarations))))

       ((member next-text '("prefix" "postfix" "infix"))
        (setq last-class-token nil)
        (setq next-token
              (swift-mode:forward-token-or-list-except-curly-bracket))
        (when (equal (swift-mode:token:text next-token) "operator")
          (setq name-token
                (swift-mode:forward-token-or-list-except-curly-bracket))
          (when (eq (swift-mode:token:type name-token) 'identifier)
            (push
             (swift-mode:declaration 'operator name-token nil)
             declarations))))

       ((equal next-text "precedencegroup")
        (setq last-class-token nil)
        (setq name-token
              (swift-mode:forward-token-or-list-except-curly-bracket))
        (when (eq (swift-mode:token:type name-token) 'identifier)
          (push
           (swift-mode:declaration 'precedencegroup name-token nil)
           declarations)))))
    declarations))

(defun swift-mode:forward-token-or-list-except-curly-bracket ()
  "Move point to the end position of the next token or list.

Curly brackets are not regarded as a list.
Return the token skipped."
  (let ((next-token (swift-mode:forward-token)))
    (if (or (memq (swift-mode:token:type next-token) '(\( \[))
            (equal (swift-mode:token:text next-token) "<"))
        (progn
          (goto-char (swift-mode:token:start next-token))
          (swift-mode:forward-token-or-list))
      next-token)))

(defun swift-mode:scan-declarations:handle-struct-like (keyword-token)
  "Parse struct-like declaration.

Return a declaration if it have a name.  Return nil otherwise.
KEYWORD-TOKEN is the keyword beginning the declaration like \"struct\" or
\"enum\"."
  (let (next-token
        (name-token (swift-mode:forward-token)))
    (when (eq (swift-mode:token:type name-token) 'identifier)
      (while (progn
               (setq next-token
                     (swift-mode:forward-token-or-list-except-curly-bracket))
               (not (memq (swift-mode:token:type next-token)
                          '(\; implicit-\; { } outside-of-buffer)))))
      (swift-mode:declaration
       (intern (swift-mode:token:text keyword-token))
       name-token
       (when (eq (swift-mode:token:type next-token) '{)
         (nreverse (swift-mode:scan-declarations)))))))

(defun swift-mode:scan-declarations:handle-case-or-variable (type)
  "Parse enum-case, let, or var.

Return a list of declarations.
TYPE is one of `case', `let', or `var'."
  ;; case A, B(String), C
  ;; case A, B = 2, C
  ;;
  ;; let x = 1,
  ;;     y = 2,
  ;;     z = 3
  ;;
  ;; var x {
  ;;   get {
  ;;     return 1
  ;;   }
  ;; }
  ;;
  ;; var x {
  ;;   willSet {
  ;;   }
  ;; }
  ;;
  ;; let (x, y) = (1, 2) // not supported yet
  (let (next-token
        (items '()))
    (while
        (progn
          (setq next-token (swift-mode:forward-token-or-list))
          (when (eq (swift-mode:token:type next-token) 'identifier)
            (push (swift-mode:declaration type next-token nil) items))
          (while
              (progn
                (setq next-token (swift-mode:forward-token-or-list))
                (not (memq (swift-mode:token:type next-token)
                           '(\, \; implicit-\; } outside-of-buffer)))))
          (eq (swift-mode:token:type next-token) '\,)))
    (when (eq (swift-mode:token:type next-token) '})
      (goto-char (swift-mode:token:start next-token)))
    items))

(defun swift-mode:scan-function-name-and-parameter-names ()
  "Parse function name and parameter names.

The point is assumed to be before a function name.

Return tokens of function names and parameter names.

For example, given the following code, this return tokens \"foo\", \"a\",
and \"c\".

  func foo(a b: Int, c: Int)"
  (let* ((name-token
         (swift-mode:forward-token-or-list-except-curly-bracket))
         next-token
         parameter-end
         (parameter-names '())
         (is-operator (seq-contains "/=-+!*%<>&|^~?."
                                    (elt (swift-mode:token:text name-token) 0))))
    (cond
     ((eq (swift-mode:token:type name-token) 'identifier)
      (while (progn
               (setq next-token
                     (swift-mode:forward-token-or-list-except-curly-bracket))
               (not (memq (swift-mode:token:type next-token)
                          '(\(\) \( { \; implicit-\; outside-of-buffer)))))
      (if (eq (swift-mode:token:type next-token) '\(\))
          (progn
            (setq parameter-end (swift-mode:token:end next-token))
            (goto-char (swift-mode:token:start next-token))
            (swift-mode:forward-token)

            (while (< (point) parameter-end)
              (setq next-token (swift-mode:forward-token))

              (when (eq (swift-mode:token:type next-token) 'identifier)
                (when (or is-operator
                          (and (equal (swift-mode:token:text name-token)
                                      "subscript")
                               (eq (swift-mode:token:type
                                    (swift-mode:forward-token-or-list))
                                   ':)))
                  (setq next-token (swift-mode:token
                                    'identifier
                                    "_"
                                    (swift-mode:token:start next-token)
                                    (swift-mode:token:end next-token))))
                (push next-token parameter-names))

              (while (and (< (point) parameter-end)
                          (not (eq (swift-mode:token:type next-token) '\,)))
                (setq next-token (swift-mode:forward-token-or-list))))
            (cons name-token (reverse parameter-names)))
        (list name-token)))
     (t nil))))

(defun swift-mode:format-for-imenu:flat (declarations)
  "Convert list of DECLARATIONS to alist for `imenu--index-alist'.

Declarations are organized as trees."
  (seq-mapcat
   (lambda (declaration)
     (let* ((name-token (swift-mode:declaration:name-token declaration))
            (name (swift-mode:token:text name-token))
            (position (swift-mode:token:start name-token))
            (children (swift-mode:declaration:children declaration)))
       (cons
        (cons name position)
        (mapcar
         (lambda (pair)
           (cons (concat name "." (car pair)) (cdr pair)))
         (swift-mode:format-for-imenu:flat children)))))
   declarations))

(defun swift-mode:format-for-imenu:nested (declarations)
  "Convert list of DECLARATIONS to alist for `imenu--index-alist'.

Declarations are organized as a flat list of fully qualified names."
  (mapcar
   (lambda (declaration)
     (let* ((name-token (swift-mode:declaration:name-token declaration))
            (name (swift-mode:token:text name-token))
            (position (swift-mode:token:start name-token))
            (children (swift-mode:declaration:children declaration)))
       (if children
           (cons name (cons (cons "self"  position)
                            (swift-mode:format-for-imenu:nested children)))
         (cons name position))))
   declarations))

(provide 'swift-mode-imenu)

;;; swift-mode-imenu.el ends here
