;;; swift-mode-font-lock.el --- Major-mode for Apple's Swift programming language, Font Locks. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021 taku0, Chris Barrett, Bozhidar Batsov,
;;                         Arthur Evstifeev, Michael Sanders

;; Author: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;       Michael Sanders <https://github.com/msanders>

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

(require 'swift-mode-lexer)
(require 'swift-mode-standard-types)
(require 'seq)
(require 'subr-x)

;;; Customizations

;;;###autoload
(defgroup swift-mode:faces nil
  "Font faces."
  :group 'swift)

(defcustom swift-mode:highlight-symbols-in-standard-library
  t
  "Highlight symbols in the standard library."
  :type 'boolean
  :safe #'booleanp)

(defcustom swift-mode:highlight-symbols-in-foundation-framework
  t
  "Highlight symbols in the Foundation framework."
  :type 'boolean
  :safe #'booleanp)

(defface swift-mode:constant-keyword-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for highlighting constant keywords.

That is, true, false, and nil.")

(defface swift-mode:preprocessor-keyword-face
  '((t . (:inherit font-lock-preprocessor-face)))
  "Face for highlighting preprocessor keywords.

Example: #if, #endif, and #selector.")

(defface swift-mode:keyword-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for highlighting keywords.")

(defface swift-mode:builtin-method-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods with trailing closure.")

(defface swift-mode:builtin-method-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods.")

(defface swift-mode:builtin-function-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions with trailing closure.")

(defface swift-mode:builtin-function-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions.")

(defface swift-mode:builtin-property-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin properties.")

(defface swift-mode:builtin-constant-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin constants.")

(defface swift-mode:builtin-enum-case-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin enum cases.")

(defface swift-mode:build-config-keyword-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting build configuration keywords.")

(defface swift-mode:builtin-type-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin types.")

(defface swift-mode:builtin-precedence-group-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin precedence groups.")

(defface swift-mode:function-call-face
  `((t . (:inherit ,(if (facep 'font-lock-function-call-face)
                        'font-lock-function-call-face
                      'font-lock-function-name-face))))
  "Face for highlighting function calls.")

(defface swift-mode:function-name-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for highlighting function names.")

(defface swift-mode:property-access-face
  `((t . (:inherit ,(if (facep
                         'font-lock-property-use-face)
                        'font-lock-property-use-face
                      'font-lock-variable-name-face))))
  "Face for highlighting property accesses.")

(defface swift-mode:negation-char-face
  '((t . (:inherit font-lock-negation-char-face)))
  "Face for highlighting the negation char.")

(defface swift-mode:comment-delimiter-face
  '((t . (:inherit font-lock-comment-delimiter-face)))
  "Face for highlighting the comment delimiters.")

(defface swift-mode:comment-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for highlighting the comments.")

(defface swift-mode:doc-face
  '((t . (:inherit font-lock-doc-face)))
  "Face for highlighting the documentation strings.")

(defface swift-mode:doc-markup-face
  `((t . (:inherit ,(if (facep
                         'font-lock-doc-markup-face)
                        'font-lock-doc-markup-face
                      'font-lock-doc-face))))
  "Face for highlighting the documentation markup.")

(defface swift-mode:type-face
  '((t . (:inherit font-lock-type-face)))
  "Face for highlighting the types.")

(defface swift-mode:string-face
  '((t . (:inherit font-lock-string-face)))
  "Face for highlighting the strings.")

(defface swift-mode:escaped-identifier-face
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for highlighting the escaped/raw identifiers.")

(defface swift-mode:delimiter-face
  `((t . ,(when (facep 'font-lock-delimiter-face)
            '(:inherit font-lock-delimiter-face))))
  "Face for highlighting the delimiters.")

(defface swift-mode:escape-face
  `((t . ,(when (facep 'font-lock-escape-face)
            '(:inherit font-lock-escape-face))))
  "Face for highlighting the escapes.")

(defface swift-mode:misc-punctuation-face
  `((t . ,(when (facep 'font-lock-misc-punctuation-face)
            '(:inherit font-lock-misc-punctuation-face))))
  "Face for highlighting the misc punctuations.")

(defface swift-mode:number-face
  `((t . ,(when (facep 'font-lock-number-face)
            '(:inherit font-lock-number-face))))
  "Face for highlighting the numbers.")

(defface swift-mode:operator-face
  `((t . ,(when (facep 'font-lock-operator-face)
            '(:inherit font-lock-operator-face))))
  "Face for highlighting the operators.")

(defface swift-mode:bracket-face
  `((t . ,(when (facep 'font-lock-bracket-face)
            '(:inherit font-lock-bracket-face))))
  "Face for highlighting the brackets.")

(defface swift-mode:regexp-face
  `((t . ,(when (facep 'font-lock-regexp-face)
            '(:inherit font-lock-regexp-face))))
  "Face for highlighting the regexps.")

(defface swift-mode:attribute-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for highlighting the attributes.")


;;; Variables

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

(defun swift-mode:skip-identifier-backward ()
  "Move point before the preceding identifier if any.

Skip also comments and spaces between point and the identifier.

Keep point if point is not after an identifier."
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((eq (char-before) ?`)
      (backward-char)
      (swift-mode:beginning-of-string)
      t)

     ((and (char-before) (memq (char-syntax (char-before)) '(?w ?_)))
      (skip-syntax-backward "w_")
      (when (eq (char-before) ?$)
        (backward-char))
      t)

     (t
      (goto-char pos)
      nil))))

(defun swift-mode:declared-function-name-pos-p (pos limit)
  "Return t if POS is just before the name of a function declaration.

This function does not search beyond LIMIT."
  (goto-char pos)
  (while (and (progn
                (forward-comment (- (point)))
                (eq (char-before) ?.))
              (progn
                (backward-char)
                (swift-mode:skip-identifier-backward))))
  (forward-comment (- (point)))
  (skip-syntax-backward "w_")
  (and
   (< (point) limit)
   (looking-at "\\_<func\\_>")))

(defun swift-mode:property-access-pos-p (pos limit)
  "Return t if POS is just before the property name of a member expression.

This function does not search beyond LIMIT."
  ;; foo.bar    // property access
  ;; foo .bar   // property access
  ;; foo . bar  // property access
  ;; foo. bar   // INVALID, but highlight as a property access anyway
  ;; foo?.bar   // property access
  ;; foo?. bar  // INVALID, but highlight as a property access anyway
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
  ;; foo.x<A>   // property access, but highlight as a method call for now
  ;; foo.x < A, B > (1) // property access
  (and
   ;; After dot
   (progn
     (goto-char pos)
     (forward-comment (- (point)))
     (eq (char-before) ?.))

   ;; Not floating-point literal
   (progn
     (backward-char)
     (let ((pos-before-dot (point)))
       (forward-comment (- (point)))
       (when (zerop (skip-syntax-backward "w_"))
         (goto-char pos-before-dot))
       (not (looking-at "[0-9]*\\.[0-9]+\\>"))))

   ;; Not method/function call
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (and
      (not (eq (char-after) ?<))
      (progn
        ;; I don't sure we can use `forward-comment' beyond limit, so assuming
        ;; no comments here.
        (skip-syntax-forward " " limit)
        (not (eq (char-after) ?\()))))))

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
   (progn
     (forward-comment (- (point)))
     (eq (char-before pos) ?.))
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-method-trailing-closure-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin method name in NAMES.

It must followed by open curly bracket.
This function does not search beyond LIMIT."
  (and
   (progn
     (forward-comment (- (point)))
     (eq (char-before pos) ?.))
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
   (progn
     (forward-comment (- (point)))
     (eq (char-before pos) ?.))
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     (or
      (eq (char-after) ?<)
      (progn
        (skip-syntax-forward " " limit)
        (eq (char-after) ?\())))
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
     (or
      (eq (char-after) ?<)
      (progn
        (skip-syntax-forward " " limit)
        (eq (char-after) ?\())))
   (swift-mode:builtin-name-pos-p names pos limit)))

(defun swift-mode:builtin-constant-name-pos-p (names pos limit)
  "Return t if POS is just before a builtin constant name in NAMES.

This function does not search beyond LIMIT."
  (swift-mode:builtin-name-pos-p names pos limit))

(defun swift-mode:font-lock-match-expr (limit match-p)
  "Move the cursor just after an identifier that satisfy given predicate.

Set `match-data', and return t if the identifier found before position LIMIT.
Return nil otherwise.

Don't match escapes/raw identifiers.

The predicate MATCH-P is called with two arguments:
- the position of the identifier, and
- the limit of search functions."
  (let ((result nil))
    (while (and
            (progn
              (dolist (key '(swift-mode:comment
                             swift-mode:string
                             swift-mode:regexp))
                (when (get-text-property (point) key)
                  (goto-char (next-single-property-change
                              (point)
                              key nil limit))))
              (< (point) limit))
            (not result)
            (re-search-forward "\\_<\\(?:\\sw\\|\\s_\\)+\\_>" limit t))
      (when (save-excursion
              (save-match-data
                (funcall match-p (match-beginning 0) limit)))
        (setq result t)))
    result))

(defun swift-mode:font-lock-match-declared-function-names (limit)
  "Move the cursor just after a function name of a function declaration.

Set `match-data', and return t if a function name found before position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-expr
   limit #'swift-mode:declared-function-name-pos-p))

(defun swift-mode:font-lock-match-property-access (limit)
  "Move the cursor just after a property access.
Set `match-data', and return t if a property access found before position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-expr limit #'swift-mode:property-access-pos-p))

(defmacro swift-mode:font-lock-match-builtin-names (f limit &rest list-of-sets)
  "Move the cursor just after a builtin name satisfying predicate F.

Predicate F takes set of names, position, and limit.

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

(defun swift-mode:negation-operator-pos-p (pos _limit)
  "Return non-nil if POS is just before a negation operator.

Return nil otherwise.

Assuming POS is just before ! character."
  (goto-char pos)
  (and (or (memq (char-before) '(nil ?\s ?\t ?\n ?\( ?\[ ?{ ?, ?\; ?:))
           (forward-comment -1))
       (not (memq (char-after (1+ (point)))
                  ;; TODO Unicode operators
                  '(nil ?\s ?\t ?\n ?\) ?\] ?} ?, ?\; ?: ?/ ?= ?- ?+ ?! ?* ?%
                        ?< ?> ?& ?| ?^ ?? ?~)))))

(defun swift-mode:font-lock-match-negation (limit)
  "Search a negation operator and return non-nil if found.

Return nil otherwise.

Do not search beyond LIMIT."
  (let ((result nil))
    (while (and
            (< (point) limit)
            (not result)
            (search-forward "!" limit t))
      (when (save-excursion
              (save-match-data
                (swift-mode:negation-operator-pos-p (match-beginning 0) limit)))
        (setq result t)))
    result))

(defconst swift-mode:syntactic-fontification-mapping
  '((swift-mode:comment swift-mode:comment-face)
    (swift-mode:documentation swift-mode:doc-face)
    (swift-mode:comment-delimiter swift-mode:comment-delimiter-face)
    (swift-mode:string swift-mode:string-face)
    (swift-mode:regexp swift-mode:regexp-face)
    (swift-mode:escaped-identifier swift-mode:escaped-identifier-face)
    (swift-mode:escape-sequence swift-mode:escape-face t)
    (swift-mode:matching-parenthesis swift-mode:misc-punctuation-face t))
  "Mapping from text properties to faces for syntactic fontification.

This is a list with elements of tuple (PROPERTY FACE [PREPEND]).

PROPERTY is the property name, FACE is the face to apply.

If PREPEND is non-nil, the face is prepended.  Otherwise, the face is
overriden.")

(defun swift-mode:fontify-syntactically (start end &optional _loudly)
  "Fontify strings and comments based on text properties set by lexer.

Fontify the region from START to END."
  (syntax-propertize end)
  (goto-char start)
  (let ((current start)
        (old-text-properties (text-properties-at (point)))
        key
        face
        prepend
        old-face
        in-documetation
        start-of-documentation)
    (while (< (point) end)
      (goto-char (next-property-change (point) nil end))
      (dolist (tuple swift-mode:syntactic-fontification-mapping)
        (setq key (nth 0 tuple))
        (setq face (nth 1 tuple))
        (setq prepend (nth 2 tuple))
        (when (and (plist-get old-text-properties key)
                   (get-text-property (1- (point)) key))
          (if prepend
              (progn
                (setq old-face (get-text-property current 'face))
                (unless (or (null old-face) (consp old-face))
                  (setq old-face (list old-face)))
                (put-text-property current (point) 'face (cons face old-face)))
            (put-text-property current (point) 'face face))
          (put-text-property current (point) 'fontified t)))
      (setq current (point))
      (setq old-text-properties (text-properties-at (point))))
    (goto-char start)
    (while (< (point) end)
      (setq in-documetation (get-text-property
                             (point)
                             'swift-mode:documentation))
      (when in-documetation
        (setq start-of-documentation (point)))
      (goto-char (next-single-property-change
                  (point)
                  'swift-mode:documentation nil end))
      (when in-documetation
        (swift-mode:fontify-documentation-markups
         start-of-documentation
         (point))))))

(defun swift-mode:fontify-syntactically-advice
    (oldfun start end &optional loudly)
  "Fontify syntactically if the current major mode is `swift-mode'.

Otherwise, call OLDFUN with START, END, and LOUDLY instead.

Intended for an advice for `font-lock-fontify-syntactically-region' for
Emacs 24. For newer Emacs, set `font-lock-fontify-syntactically-function' to
`swift-mode:fontify-syntactically' instead."
  (if (eq major-mode 'swift-mode)
      (swift-mode:fontify-syntactically start end loudly)
    (funcall oldfun start end loudly)))

(defconst swift-mode:documentation-tags
  ;; https://github.com/swiftlang/swift-docc/blob/2cff0b04bd64d9be5803b0abacdbd4078157cadc/Sources/SwiftDocC/Utility/MarkupExtensions/ListItemExtractor.swift#L341
  '(("returns" . nil)
    ("throws" . nil)
    ("parameter" . t)
    ("parameters" . nil)
    ("dictionarykey" . t)
    ("dictionarykeys" . nil)
    ("possiblevalue" . t)
    ("possiblevalues" . nil)
    ("httpbody" . nil)
    ("httpresponse" . t)
    ("httpresponses" . nil)
    ("httpparameter" . t)
    ("httpparameters" . nil)
    ("httpbodyparameter" . t)
    ("httpbodyparameters" . nil)
    ("attention" . nil)
    ("author" . nil)
    ("authors" . nil)
    ("bug" . nil)
    ("complexity" . nil)
    ("copyright" . nil)
    ("date" . nil)
    ("experiment" . nil)
    ("important" . nil)
    ("invariant" . nil)
    ("localizationkey" . nil)
    ("mutatingvariant" . nil)
    ("nonmutatingvariant" . nil)
    ("note" . nil)
    ("postcondition" . nil)
    ("precondition" . nil)
    ("remark" . nil)
    ("remarks" . nil)
    ("requires" . nil)
    ("seealso" . nil)
    ("since" . nil)
    ("tag" . nil)
    ("todo" . nil)
    ("version" . nil)
    ("warning" . nil)
    ("keyword" . nil)
    ("recommended" . nil)
    ("recommendedover" . nil))
  "Tags of documentation markups.

This is a list of cons (TAG . HAVE-ARG).

TAG is lowercased tag name.

If HAVE-ARG is non-nil, the tag must be followed by arguments.
If HAVE-ARG is nil, the tag must not be followed by arguments.")

(defun swift-mode:fontify-documentation-markups (start end)
  "Fontify documentation markups in documentation comments.

Fontify the region from START to END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward
            "-[\s\t]+\\([^\s\t:]*\\)\\(?:[\s\t]+\\([^:]+\\)\\)?:"
            end
            t)
      (let ((tag (downcase (match-string-no-properties 1)))
            (name (match-string-no-properties 2)))
        ;; FIXME Use hash table
        (dolist (tuple swift-mode:documentation-tags)
          (when (and (equal tag (car tuple))
                     (eq (cdr tuple) (not (not name))))
            (put-text-property (match-beginning 1) (match-end 1)
                               'face 'swift-mode:doc-markup-face)))))))


;;; Keywords and standard identifiers

;; Keywords
;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410

(defconst swift-mode:constant-keywords
  '("true" "false" "nil")
  "Keywords used as constants.")

(defconst swift-mode:declaration-keywords
  '("associatedtype" "class" "deinit" "enum" "extension" "fileprivate" "func"
    "import" "init" "inout" "internal" "let" "open" "operator" "package"
    "private" "protocol" "public" "any" "some" "static" "struct" "subscript"
    "typealias" "var" "actor" "nonisolated" "isolated" "distributed"
    "borrowing" "consuming" "sending" "macro")
  "Keywords used in declarations.")

(defconst swift-mode:statement-keywords
  '("break" "case" "continue" "default" "defer" "do" "else" "fallthrough" "for"
    "guard" "if" "in" "repeat" "return" "switch" "where" "while")
  "Keywords used in statements.")

(defconst swift-mode:expression-keywords
  '("as" "catch" "dynamicType" "is" "rethrows" "super" "self" "Self" "throws"
    "throw" "try" "async" "await" "consume" "copy" "discard" "each")
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
    "i386" "x86_64" "arm" "arm64"
    "OSX" "OSXApplicationExtension"
    "macOS" "macOSApplicationExtension"
    "iOS" "iOSApplicationExtension"
    "watchOS" "watchOSApplicationExtension"
    "tvOS" "tvOSApplicationExtension"
    "macCatalyst" "macCatalystApplicationExtension"
    "Linux" "Windows"
    "simulator" "unavailable" "noasync"
    "hasFeature" "hasAttribute" "before" "introduced" "deprecated" "obsoleted"
    "message" "renamed")
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
    ("@\\(?:\\sw\\|\\s_\\)*"
     .
     'swift-mode:attribute-face)

    (,(regexp-opt swift-mode:constant-keywords 'symbols)
     .
     'swift-mode:constant-keyword-face)

    ;; Preprocessor keywords
    ("#\\(?:\\sw\\|\\s_\\)*"
     .
     'swift-mode:preprocessor-keyword-face)

    (,(regexp-opt (append swift-mode:declaration-keywords
                          swift-mode:statement-keywords
                          swift-mode:expression-keywords
                          swift-mode:context-keywords)
                  'symbols)
     .
     'swift-mode:keyword-face)

    ;; Numbers
    (,(concat
       ;; Optional sign
       "-?"
       "\\_<\\(?:"
       ;; Decimal integer/floating-point
       "[0-9][0-9_]*"
       "\\(?:\\.[0-9][0-9_]*\\)?"
       "\\(?:[eE][+-]?[0-9][0-9_]*\\)?"
       "\\|"
       ;; Hexadecimal integer/floating-point
       "0x[0-9a-fA-F][0-9a-fA-F_]*"
       "\\(?:\\.[0-9a-fA-F][0-9a-fA-F_]*\\)?"
       "\\(?:[pP][+-]?[0-9][0-9_]*\\)?"
       "\\|"
       ;; Binary interger
       "0b[01][01_]*"
       "\\|"
       ;; Octal interger
       "0o[0-7][0-7_]*"
       "\\)\\_>")
     .
     'swift-mode:number-face)

    ;; Negation operator
    (swift-mode:font-lock-match-negation
     .
     'swift-mode:negation-char-face)

    ;; Other operators
    ;; TODO Unicode operators
    ("[/=+!*%<>&|^?~.-]+"
     .
     'swift-mode:operator-face)

    ;; Colon of ternary conditional operator
    ;; HEURISTICS: if it has spaces on left, treat it as an operator rather
    ;; than a delimiter.
    ("\\(?:^\\|[\s\t([{,;:]\\)\\(:\\)"
     1
     'swift-mode:operator-face)

    ;; Delimiters
    ("[,:;]"
     .
     'swift-mode:delimiter-face)

    ;; Brackets
    ("[][(){}]"
     .
     'swift-mode:bracket-face)

    ;; Builtin names
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

    (,(regexp-opt swift-mode:build-config-keywords 'symbols)
     .
     'swift-mode:build-config-keyword-face)

    (,(regexp-opt swift-mode:standard-precedence-groups 'symbols)
     .
     'swift-mode:builtin-precedence-group-face)

    ;; Type names
    (,"\\_<[A-Z]\\(?:\\sw\\|\\s_\\)*\\_>"
     .
     'swift-mode:type-face)

    ;; Function declarations
    (swift-mode:font-lock-match-declared-function-names
     .
     'swift-mode:function-name-face)

    ;; Method/function calls
    ("\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>\\??\\(?:<\\|\\s-*(\\|\\s-*{\\)"
     1
     'swift-mode:function-call-face)

    ;; Property accesses
    (swift-mode:font-lock-match-property-access
     .
     'swift-mode:property-access-face))
  "Swift mode keywords for Font Lock.")


(provide 'swift-mode-font-lock)

;;; swift-mode-font-lock.el ends here
