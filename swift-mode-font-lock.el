;;; swift-mode-font-lock.el --- Major-mode for Apple's Swift programming language, Font Locks. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2018 taku0, Chris Barrett, Bozhidar Batsov,
;;                         Arthur Evstifeev, Michael Sanders

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;       Michael Sanders <michael.sanders@fastmail.com>
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

;; Routines for Font Locks

;;; Code:

(require 'swift-mode-standard-types)
(require 'seq)

;;; Customizations

;;;###autoload
(defgroup swift-mode:faces nil
  "Font faces."
  :group 'swift)

(defcustom swift-mode:highlight-symbols-in-standard-library
  t
  "Highlight symbols in the standard library."
  :type 'boolean
  :group 'swift-mode:faces
  :safe 'booleanp)

(defcustom swift-mode:highlight-symbols-in-foundation-framework
  t
  "Highlight symbols in the Foundation framework."
  :type 'boolean
  :group 'swift-mode:faces
  :safe 'booleanp)

(defface swift-mode:constant-keyword-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for highlighting constant keywords

That is, true, false, and nil."
  :group 'swift-mode:faces)

(defface swift-mode:preprocessor-keyword-face
  '((t . (:inherit font-lock-preprocessor-face)))
  "Face for highlighting preprocessor keywords.

Example: #if, #endif, and #selector."
  :group 'swift-mode:faces)

(defface swift-mode:keyword-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for highlighting keywords."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-method-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods with trailing closure."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-method-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-function-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions with trailing closure."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-function-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-property-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin properties."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-constant-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin constants."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-enum-case-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin enum cases."
  :group 'swift-mode:faces)

(defface swift-mode:build-config-keyword-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting build configuration keywords."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-type-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin types."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-precedence-group-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin precedence groups."
  :group 'swift-mode:faces)

(defface swift-mode:function-call-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for highlighting function calls."
  :group 'swift-mode:faces)

(defface swift-mode:function-name-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for highlighting function names."
  :group 'swift-mode:faces)

(defface swift-mode:property-access-face
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for highlighting property accesses."
  :group 'swift-mode:faces)


(defun swift-mode:make-set (list)
  "Return a hash where its keys are elements of the LIST.

All values are t."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (value list)
      (puthash value t hash))
    hash))

(defvar swift-mode:standard-types-hash
  (swift-mode:make-set swift-mode:standard-types)
  "Set of standard type names.  All values are t.")

(defvar swift-mode:standard-enum-cases-hash
  (swift-mode:make-set swift-mode:standard-enum-cases)
  "Set of standard enum case names.  All values are t.")

(defvar swift-mode:standard-methods-hash
  (swift-mode:make-set swift-mode:standard-methods)
  "Set of standard method names.  All values are t.")

(defvar swift-mode:standard-properties-hash
  (swift-mode:make-set swift-mode:standard-properties)
  "Set of standard property names.  All values are t.")

(defvar swift-mode:standard-functions-hash
  (swift-mode:make-set swift-mode:standard-functions)
  "Set of standard function names.  All values are t.")

(defvar swift-mode:standard-constants-hash
  (swift-mode:make-set swift-mode:standard-constants)
  "Set of standard constant names.  All values are t.")

(defvar swift-mode:foundation-types-hash
  (swift-mode:make-set swift-mode:foundation-types)
  "Set of Foundation type names.  All values are t.")

(defvar swift-mode:foundation-enum-cases-hash
  (swift-mode:make-set swift-mode:foundation-enum-cases)
  "Set of Foundation enum case names.  All values are t.")

(defvar swift-mode:foundation-methods-hash
  (swift-mode:make-set swift-mode:foundation-methods)
  "Set of Foundation method names.  All values are t.")

(defvar swift-mode:foundation-properties-hash
  (swift-mode:make-set swift-mode:foundation-properties)
  "Set of Foundation property names.  All values are t.")

(defvar swift-mode:foundation-functions-hash
  (swift-mode:make-set swift-mode:foundation-functions)
  "Set of Foundation function names.  All values are t.")

(defvar swift-mode:foundation-constants-hash
  (swift-mode:make-set swift-mode:foundation-constants)
  "Set of Foundation constant names.  All values are t.")


;;; Supporting functions

(defun swift-mode:declared-function-name-pos-p (pos limit)
  "Return t if POS is just before the name of a function declaration.

This function does not search beyond LIMIT."
  (goto-char pos)
  (forward-comment (- (point)))
  (skip-syntax-backward "w_")
  (and (< (point) limit)
       (looking-at
        "\\<\\(func\\|enum\\|struct\\|class\\|protocol\\|extension\\)\\>")))

(defun swift-mode:property-access-pos-p (pos limit)
  "Return t if POS is just before the property name of a member expression.

This function does not search beyond LIMIT."
  ;; foo.bar    // property access
  ;; foo .bar   // property access
  ;; foo . bar  // INVALID
  ;; foo. bar   // INVALID
  ;; foo?.bar   // property access
  ;; foo?. bar  // INVALID
  ;; foo ?.bar  // INVALID, but highlight as a property access anyway
  ;; foo? .bar  // property access
  ;; foo.bar()  // NOT property access
  ;; foo.bar () // NOT property access
  ;; foo.1      // property access
  ;; foo1.1     // property access
  ;; 1.1        // NOT property access
  ;; .1         // NOT property access
  ;; $1.1       // property access
  ;; .x         // property access
  (and
   ;; Just after dot
   (progn
     (goto-char pos)
     (eq (char-before) ?.))

   ;; Not floating-point literal
   (progn
     (goto-char pos)
     (backward-char)
     (skip-syntax-backward "w_")
     (not (looking-at "[0-9]*\\.[0-9]+\\>")))

   ;; Not method/function call
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     ;; I don't sure we can use `forward-comment' beyond limit, so assuming
     ;; no comments here.
     (skip-syntax-forward " " limit)
     (not (eq (char-after) ?\()))))

(defun swift-mode:builtin-name-pos-p (names pos limit)
  "Return t if an identifier in the hash NAMES appears at POS.

This function does not search beyond LIMIT."
  (goto-char pos)
  (skip-syntax-forward "w_" limit)
  (gethash (buffer-substring-no-properties pos (point)) names))

(defun swift-mode:builtin-type-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin type name in NAMES.

This function does not search beyond LIMIT."
  (swift-mode:builtin-name-pos-p names pos limit))

(defun swift-mode:builtin-enum-case-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin enum case name in NAMES.

This function does not search beyond LIMIT."
  (and
   (eq (char-before pos) ?.)
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-method-trailing-closure-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin method name in NAMES.

It must followed by open curly bracket.
This function does not search beyond LIMIT."
  (and
   (eq (char-before pos) ?.)
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?{))
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-method-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin method name in NAMES.

This function does not search beyond LIMIT."
  (and
   (eq (char-before pos) ?.)
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?\())
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-property-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin property name in NAMES.

This function does not search beyond LIMIT."
  (and
   (swift-mode:property-access-pos-p pos limit)
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-function-trailing-closure-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin function name in NAMES.

It must followed by open curly bracket.
This function does not search beyond LIMIT."
  (and
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?{))
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-function-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin function name in NAMES.

This function does not search beyond LIMIT."
  (and
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (skip-syntax-forward " " limit)
     (eq (char-after) ?\())
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-constant-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin constant name in NAMES.

This function does not search beyond LIMIT."
  (swift-mode:builtin-name-pos-p names pos limit))

(defun swift-mode:font-lock-match-expr (limit match-p)
  "Move the cursor just after an identifier that satisfy given predicate.

Set `match-data', and return t if the identifier found before position LIMIT.
Return nil otherwise.

The predicate MATCH-P is called with two arguments:
- the position of the identifier, and
- the limit of search functions."
  (and
   (< (point) limit)
   (re-search-forward "\\<\\(\\sw\\|\\s_\\)+\\>" limit t)
   (or
    (save-excursion
      (save-match-data
        (funcall match-p (match-beginning 0) limit)))
    (swift-mode:font-lock-match-expr limit match-p))))

(defun swift-mode:font-lock-match-declared-function-names (limit)
  "Move the cursor just after a function name or others.

Others includes enum, struct, class, protocol, and extension name.
Set `match-data', and return t if a function name or others found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-expr
   limit #'swift-mode:declared-function-name-pos-p))

(defun swift-mode:font-lock-match-property-access (limit)
  "Move the cursor just after a property access.
Set `match-data', and return t if a property access found before position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-expr limit #'swift-mode:property-access-pos-p))

(defmacro swift-mode:font-lock-match-builtin-names (f limit &rest list-of-sets)
  "Move the cursor just after a builtin name.

Function F takes set of names, position, and limit.

Set `match-data', and return t if a builtin name found before position LIMIT.
Return nil otherwise.

LIST-OF-SETS is a list of set of names."
  (let ((pos (make-symbol "pos"))
        (limit2 (make-symbol "limit"))
        (matched (make-symbol "matched"))
        (names (make-symbol "names")))
  `(swift-mode:font-lock-match-expr
    ,limit
    (lambda (,pos ,limit2)
      (seq-reduce
       (lambda (,matched ,names)
         (or ,matched
             (and ,names
                  (funcall ,f ,names ,pos ,limit2))))
       (list ,@list-of-sets)
       nil)))))

(defun swift-mode:font-lock-match-builtin-type-names (limit)
  "Move the cursor just after a builtin type name.

Set `match-data', and return t if a builtin type name found before position
LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-type-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-types-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-types-hash)))

(defun swift-mode:font-lock-match-builtin-enum-case-names (limit)
  "Move the cursor just after a builtin enum case name.

Set `match-data', and return t if a builtin enum case name found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-enum-case-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-enum-cases-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-enum-cases-hash)))

(defun swift-mode:font-lock-match-builtin-method-trailing-closure-names (limit)
  "Move the cursor just after a builtin method name with trailing closure.

Set `match-data', and return t if a builtin method name found before position
LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-method-trailing-closure-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-methods-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-methods-hash)))

(defun swift-mode:font-lock-match-builtin-method-names (limit)
  "Move the cursor just after a builtin method name.

Set `match-data', and return t if a builtin method name found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-method-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-methods-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-methods-hash)))

(defun swift-mode:font-lock-match-builtin-property-names (limit)
  "Move the cursor just after a builtin property name.

Set `match-data', and return t if a builtin property name found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-property-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-properties-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-properties-hash)))

(defun swift-mode:font-lock-match-builtin-function-trailing-closure-names
    (limit)
  "Move the cursor just after a builtin function name with trailing closure.

Set `match-data', and return t if a builtin function name found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-function-trailing-closure-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-functions-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-functions-hash)))

(defun swift-mode:font-lock-match-builtin-function-names (limit)
  "Move the cursor just after a builtin function name.

Set `match-data', and return t if a builtin function name found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-function-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-functions-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-functions-hash)))

(defun swift-mode:font-lock-match-builtin-constant-names (limit)
  "Move the cursor just after a builtin constant name.

Set `match-data', and return t if a builtin constant name found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-builtin-names
   #'swift-mode:builtin-constant-name-pos-p
   limit
   (and swift-mode:highlight-symbols-in-standard-library
        swift-mode:standard-constants-hash)
   (and swift-mode:highlight-symbols-in-foundation-framework
        swift-mode:foundation-constants-hash)))

;;; Keywords and standard identifiers

;; Keywords
;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410

(defconst swift-mode:constant-keywords
  '("true" "false" "nil")
  "Keywords used as constants.")

(defconst swift-mode:preprocessor-keywords
  '("#available" "#colorLiteral" "#column" "#dsohandle" "#else" "#elseif"
    "#endif" "#error" "#file" "#fileLiteral" "#function" "#if" "#imageLiteral"
    "#keyPath" "#line" "#selector" "#sourceLocation" "#warning")
  "Keywords that begin with a number sign (#).")

(defconst swift-mode:declaration-keywords
  '("associatedtype" "class" "deinit" "enum" "extension" "fileprivate" "func"
    "import" "init" "inout" "internal" "let" "open" "operator" "private"
    "protocol" "public" "static" "struct" "subscript" "typealias" "var")
  "Keywords used in declarations.")

(defconst swift-mode:statement-keywords
  '("break" "case" "continue" "default" "defer" "do" "else" "fallthrough" "for"
    "guard" "if" "in" "repeat" "return" "switch" "where" "while")
  "Keywords used in statements.")

(defconst swift-mode:expression-keywords
  '("as" "catch" "dynamicType" "is" "rethrows" "super" "self" "Self" "throws"
    "throw" "try")
  "Keywords used in expressions and types.

Excludes true, false, and keywords begin with a number sign.")

(defconst swift-mode:context-keywords
  '("Protocol" "Type" "and" "assignment" "associativity" "convenience" "didSet"
    "dynamic" "final" "get" "higherThan" "indirect" "infix" "lazy" "left"
    "lowerThan" "mutating" "none" "nonmutating" "optional" "override" "postfix"
    "precedence" "precedencegroup" "prefix" "required" "right" "set" "unowned"
    "weak" "willSet")
  "Keywords reserved in particular contexts.")

(defconst swift-mode:build-config-keywords
  '("os" "arch" "swift" "compiler" "canImport" "targetEnvironment"
    "OSX" "macOS" "iOS" "watchOS" "tvOS" "i386" "x86_64" "arm" "arm64"
    "iOSApplicationExtension" "OSXApplicationExtension"
    "macOSApplicationExtension" "simulator")
  "Keywords for build configuration statements.")

(defconst swift-mode:standard-precedence-groups
  '("AssignmentPrecedence"
    "FunctionArrowPrecedence"
    "TernaryPrecedence"
    "DefaultPrecedence"
    "LogicalDisjunctionPrecedence"
    "LogicalConjunctionPrecedence"
    "ComparisonPrecedence"
    "NilCoalescingPrecedence"
    "CastingPrecedence"
    "RangeFormationPrecedence"
    "AdditionPrecedence"
    "MultiplicationPrecedence"
    "BitwiseShiftPrecedence")
  "Precedence groups in the standard library.")

;;; font-lock definition

(defconst swift-mode:font-lock-keywords
  `(
    ;; Attributes
    "@\\(\\sw\\|\\s_\\)*"

    (,(regexp-opt swift-mode:constant-keywords 'words)
     .
     'swift-mode:constant-keyword-face)

    (,(regexp-opt swift-mode:preprocessor-keywords 'symbols)
     .
     'swift-mode:preprocessor-keyword-face)

    (,(regexp-opt (append swift-mode:declaration-keywords
                          swift-mode:statement-keywords
                          swift-mode:expression-keywords
                          swift-mode:context-keywords)
                  'words)
     .
     'swift-mode:keyword-face)

    (swift-mode:font-lock-match-builtin-type-names
     .
     'swift-mode:builtin-type-face)

    (swift-mode:font-lock-match-builtin-enum-case-names
     .
     'swift-mode:builtin-enum-case-face)

    (swift-mode:font-lock-match-builtin-method-trailing-closure-names
     .
     'swift-mode:builtin-method-trailing-closure-face)

    (swift-mode:font-lock-match-builtin-method-names
     .
     'swift-mode:builtin-method-face)

    (swift-mode:font-lock-match-builtin-property-names
     .
     'swift-mode:builtin-property-face)

    (swift-mode:font-lock-match-builtin-function-trailing-closure-names
     .
     'swift-mode:builtin-function-trailing-closure-face)

    (swift-mode:font-lock-match-builtin-function-names
     .
     'swift-mode:builtin-function-face)

    (swift-mode:font-lock-match-builtin-constant-names
     .
     'swift-mode:builtin-constant-face)

    (,(regexp-opt swift-mode:build-config-keywords 'words)
     .
     'swift-mode:build-config-keyword-face)

    (,(concat "\\<"
              (regexp-opt swift-mode:standard-precedence-groups 'non-nil)
              "\\>")
     .
     'swift-mode:builtin-precedence-group-face)

    ;; Method/function calls
    ("\\<\\(\\(\\sw\\|\\s_\\)+\\)\\>\\??\\s-*("
     1
     'swift-mode:function-call-face)

    ;; Function and type declarations
    (swift-mode:font-lock-match-declared-function-names
     .
     'swift-mode:function-name-face)

    ;; Property accesses
    (swift-mode:font-lock-match-property-access
     .
     'swift-mode:property-access-face))
  "Swift mode keywords for Font Lock.")


(provide 'swift-mode-font-lock)

;;; swift-mode-font-lock.el ends here
