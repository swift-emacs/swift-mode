;;; swift-mode.el --- Major-mode for Apple's Swift programming language. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.3.0-cvs
;; Package-Requires: ((emacs "24.1"))
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

;; Major-mode for Apple's Swift programming language.

;;; Code:

(require 'rx)
(require 'comint)

(eval-and-compile
  ;; Added in Emacs 24.3
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defgroup swift nil
  "Configuration for swift-mode."
  :group 'languages
  :prefix "swift-")

(defcustom swift-indent-offset 4
  "Defines the indentation offset for Swift code."
  :group 'swift
  :type 'integerp)

(defcustom swift-indent-switch-case-offset 0
  "Defines the indentation offset for cases in a switch statement."
  :group 'swift
  :type 'integerp)

(defcustom swift-repl-executable
  "xcrun swift"
  "Path to the Swift CLI."
  :group 'swift)

;;; Indentation

(require 'smie)

(defconst swift-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (type (type) (type "<T" types "T>"))
       (types (type) (type "," type))

       (class-decl-exp (id) (id ":" types))
       (decl-exp (id) (id ":" type))
       (decl-exps (decl-exp) (decl-exp "," decl-exp))

       (assign-exp (decl-exp) (id "=" exp))

       (decl (decl ";" decl))
       (decl (let-decl) (var-decl))
       (let-decl
        ("let" id ":" type)
        ("let" id "=" exp)
        ("let" id ":" type "=" exp))
       (var-decl
        ("var" id ":" type)
        ("var" id "=" exp)
        ("var" id ":" type "=" exp))

       (top-level-sts (top-level-st) (top-level-st ";" top-level-st))
       (top-level-st
        ("import" type)
        (decl)
        ("class" class-decl-exp "{" class-level-sts "}"))

       (class-level-sts (class-level-st) (class-level-st ";" class-level-st))
       (class-level-st
        (decl)
        ("DECSPEC" "func" func-header "{" insts "}"))

       (func-header (id "(" func-params ")"))
       (func-param (decl-exp) ("..."))
       (func-params (func-param "," func-param))

       (insts (inst) (insts ";" insts))
       (inst (decl)
             (in-exp)
             (dot-exp)
             (dot-exp "{" insts "}")
             (method-call)
             (method-call "{" insts "}")
             ("enum" decl-exp "{" enum-body "}")
             ("switch" exp "{" switch-body "}")
             (if-clause)
             ("for" for-head "{" insts "}")
             ("while" exp "{" insts "}"))

       (dot-exp (exp "." exp))

       (method-call (dot-exp "(" method-args ")"))
       (method-args (method-arg) (method-arg "," method-arg))
       (method-arg (exp "," "{" insts "}") (exp))

       (exp (op-exp)
            ("[" decl-exps "]"))
       (in-exp (id "in" exp))
       (guard-exp (exp "where" exp))
       (op-exp (exp "OP" exp))

       (enum-cases (assign-exp)
                   (enum-cases ";" "ecase" enum-cases))
       (enum-body (enum-cases) (insts))

       (case-exps (guard-exp))
       (cases (case-exps ":" insts)
              (cases "case" cases))
       (switch-body (cases) (cases "default" insts))

       (for-head (in-exp) (op-exp) (for-head ";" for-head))

       (conditional (exp) (let-decl))
       (if-body ("if" conditional "{" insts "}"))
       (if-else-if (if-body) (if-else-if "else" if-else-if))
       (if-clause (if-else-if)))
     ;; Conflicts
     '((nonassoc "{") (assoc ",") (assoc ";") (assoc ":"))
     '((assoc "in") (assoc "where") (assoc "OP"))
     '((assoc "else"))
     '((assoc ";") (assoc "ecase"))
     '((assoc "case")))

    (smie-precs->prec2
     '(
       (right "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&="
              "^=" "|=" "&&=" "||=" "=")                       ;; Assignment (Right associative, precedence level 90)
       (left "||")                                             ;; Disjunctive (Left associative, precedence level 110)
       (left "&&")                                             ;; Conjunctive (Left associative, precedence level 120)
       (nonassoc "<" "<=" ">" ">=" "==" "!=" "===" "!==" "~=") ;; Comparative (No associativity, precedence level 130)
       (nonassoc "is" "as" "as?")                              ;; Cast (No associativity, precedence level 132)
       (nonassoc ".." "...")                                   ;; Range (No associativity, precedence level 135)
       (left "+" "-" "&+" "&-" "|" "^")                        ;; Additive (Left associative, precedence level 140)
       (left "*" "/" "%" "&*" "&/" "&%" "&")                   ;; Multiplicative (Left associative, precedence level 150)
       (nonassoc "<<" ">>")                                    ;; Exponentiative (No associativity, precedence level 160)
       ))
    )))

(defun verbose-swift-smie-rules (kind token)
  (let ((value (swift-smie-rules kind token)))
    (message "%s '%s'; sibling-p:%s parent:%s hanging:%s == %s" kind token
             (ignore-errors (smie-rule-sibling-p))
             (ignore-errors smie--parent)
             (ignore-errors (smie-rule-hanging-p))
             value)
    value))

(defvar swift-smie--operators-regexp
  (regexp-opt '("*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|=" "&&=" "||="
                "<" "<=" ">" ">=" "==" "!=" "===" "!==" "~=" "||" "&&"
                "is" "as" "as?" ".." "..."
                "+" "-" "&+" "&-" "|" "^"
                "*" "/" "%" "&*" "&/" "&%" "&"
                "<<" ">>")))

(defvar swift-smie--decl-specifier-regexp
  (regexp-opt '("class" "mutating" "override" "static" "unowned" "weak")))

(defun swift-smie--implicit-semi-p ()
  (save-excursion
    (not (or (memq (char-before) '(?\{ ?\[ ?\,))
             (looking-back swift-smie--operators-regexp (- (point) 3) t)
             ))))

(defun swift-smie--forward-token ()
  (cond
   ((and (looking-at "\n") (swift-smie--implicit-semi-p))
    (if (eolp) (forward-char 1) (forward-comment 1))
    ";")

   ((looking-at "{") (forward-char 1) "{")
   ((looking-at "}") (forward-char 1) "}")

   ((looking-at ",") (forward-char 1) ",")

   ((looking-at "<") (forward-char 1)
    (if (looking-at "[[:upper:]]") "<T" "OP"))
   ((looking-at ">") (forward-char 1)
    (if (looking-back "[[:space:]]>" 2 t) "OP" "T>"))

   ((looking-at swift-smie--operators-regexp)
    (goto-char (match-end 0)) "OP")

   ((looking-at swift-smie--decl-specifier-regexp)
    (goto-char (match-end 0)) "DECSPEC")

   (t (let ((tok (smie-default-forward-token)))
        (cond
         ((equal tok "case")
          (if (looking-at ".+\\(,\\|:\\)")
              "case"
            "ecase"))
         (t tok))))
   ))

(defun swift-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (swift-smie--implicit-semi-p))
      ";")

     ((eq (char-before) ?\{) (backward-char 1) "{")
     ((eq (char-before) ?\}) (backward-char 1) "}")

     ((eq (char-before) ?,) (backward-char 1) ",")

     ((eq (char-before) ?<) (backward-char 1)
      (if (looking-at "<[[:upper:]]") "<T" "OP"))
     ((eq (char-before) ?>) (backward-char 1)
      (if (looking-back "[[:space:]]" 1 t) "OP" "T>"))

     ((looking-back swift-smie--operators-regexp (- (point) 3) t)
      (goto-char (match-beginning 0)) "OP")

     ((looking-back swift-smie--decl-specifier-regexp (- (point) 8) t)
      (goto-char (match-beginning 0)) "DECSPEC")

     (t (let ((tok (smie-default-backward-token)))
          (cond
           ((equal tok "case")
            (if (looking-at ".+\\(,\\|:\\)")
                "case"
              "ecase"))
           (t tok))))
     )))

(defun swift-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) swift-indent-offset)

    (`(:after . "{")
     (if (smie-rule-parent-p "switch")
         (smie-rule-parent swift-indent-switch-case-offset)))
    (`(:before . ";")
     (if (smie-rule-parent-p "case" "default")
         (smie-rule-parent swift-indent-offset)))

    (`(:before . "if")
     (if (smie-rule-prev-p "else")
         (if (smie-rule-parent-p "{")
             swift-indent-offset
           (smie-rule-parent))))

    (`(:before . "(")
     (if (smie-rule-next-p "[") (smie-rule-parent)))
    (`(:before . "[") (smie-rule-parent))
    ))

;;; Font lock

(defvar swift-mode--type-decl-keywords
  '("class" "enum" "protocol" "struct" "typealias"))

(defvar swift-mode--val-decl-keywords
  '("let" "var"))

(defvar swift-mode--context-variables-keywords
  '("self" "super"))

(defvar swift-mode--fn-decl-keywords
  '("deinit" "func" "init"))

(defvar swift-mode--misc-keywords
  '("import" "static" "subscript" "extension"))

(defvar swift-mode--statement-keywords
  '("break" "case" "continue" "default" "do" "else" "fallthrough"
    "if" "in" "for" "return" "switch" "where" "while"))

(defvar swift-mode--contextual-keywords
  '("associativity" "didSet" "get" "infix" "inout" "left" "mutating" "none"
    "nonmutating" "operator" "override" "postfix" "precedence" "prefix" "right"
    "set" "unowned" "unowned(safe)" "unowned(unsafe)" "weak" "willSet" "convenience"))

(defvar swift-mode--attribute-keywords
  '("assignment" "class_protocol" "exported" "final" "lazy" "noreturn"
    "NSCopying" "NSManaged" "objc" "optional" "required" "auto_closure"
    "IBAction" "IBDesignable" "IBInspectable" "IBOutlet"))

(defvar swift-mode--keywords
  (append swift-mode--type-decl-keywords
          swift-mode--val-decl-keywords
          swift-mode--context-variables-keywords
          swift-mode--fn-decl-keywords
          swift-mode--misc-keywords
          swift-mode--statement-keywords
          swift-mode--contextual-keywords)
  "Keywords used in the Swift language.")

(defvar swift-mode--constants
  '("true" "false" "nil"))

(defvar swift-font-lock-keywords
  `(
    ;; Keywords
    ;;
    ;; Swift allows reserved words to be used as identifiers when enclosed
    ;; with backticks, in which case they should be highlighted as
    ;; identifiers, not keywords.
    (,(rx-to-string
       `(and (or bol (not (any "`"))) bow
             (group (or ,@swift-mode--keywords))
             eow)
       t)
     1 font-lock-keyword-face)

    ;; Attributes
    ;;
    ;; Highlight attributes with keyword face
    (,(rx-to-string
       `(and "@" bow (or ,@swift-mode--attribute-keywords) eow)
       t)
     0 font-lock-keyword-face)

    ;; Types
    ;;
    ;; Any token beginning with an uppercase character is highlighted as a
    ;; type.
    (,(rx bow upper (* word) eow)
     0 font-lock-type-face)

    ;; Function names
    ;;
    ;; Any token beginning after `func' is highlighted as a function name.
    (,(rx bow "func" eow (+ space) (group bow (+ word) eow))
     1 font-lock-function-name-face)

    ;; Value bindings
    ;;
    ;; Any token beginning after `let' or `var' is highlighted as an
    ;; identifier.
    (,(rx-to-string `(and bow
                           (or ,@swift-mode--val-decl-keywords)
                           eow
                           (+ space)
                           (? "(")
                           (group (+ (or (+ (? ?`) word (? ?`)) ?, space)))
                           (? ")"))
                     t)
       1 font-lock-variable-name-face)

    ;; Use high-visibility face for pattern match wildcards.
    (,(rx (not (any word digit)) (group "_") (or eol (not (any word digit))))
     1 font-lock-negation-char-face)

    ;; Constants
    ;;
    ;; Highlight nil and boolean literals.
    (,(rx-to-string `(and bow (or ,@swift-mode--constants) eow))
     0 font-lock-constant-face)

    ;; Attributes
    ;;
    ;; Use string face for attribute name.
    (,(rx (or bol space)(group "@" (+ word)) eow)
     1 font-lock-string-face)

    ;; Imported modules
    ;;
    ;; Highlight the names of imported modules. Use `font-lock-string-face' for
    ;; consistency with C modes.
    (,(rx bow "import" eow (+ space) (group (+ word)))
     1 font-lock-string-face)

    ;; String interpolation
    ;;
    ;; Highlight interpolation expression as identifier.
    (swift-match-interpolation 0 font-lock-variable-name-face t)
    ))

(defun swift-syntax-propertize-function (start end)
  "Syntactic keywords for Swift mode."
  (let (case-fold-search)
    (goto-char start)
    (remove-text-properties start end '(swift-interpolation-match-data))
    (funcall
     (syntax-propertize-rules
      ((rx (group "\\(" (*? any) ")"))
       (0 (ignore (swift-syntax-propertize-interpolation)))))
     start end)))

(defun swift-syntax-propertize-interpolation ()
  (let* ((beg (match-beginning 0))
         (context (save-excursion (save-match-data (syntax-ppss beg)))))
    (put-text-property beg (1+ beg) 'swift-interpolation-match-data
                       (cons (nth 3 context) (match-data)))))

(defun swift-match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point) 'swift-interpolation-match-data
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'swift-interpolation-match-data)))
        (if (eq (car value) ?\")
            (progn
              (set-match-data (cdr value))
              t)
          (swift-match-interpolation limit))))))

;;; Imenu

(defun swift-mode--mk-regex-for-def (keyword)
  "Make a regex matching the identifier introduced by KEYWORD."
  (let ((ident (rx (any word nonascii "_") (* (any word nonascii digit "_")))))
    (rx-to-string `(and bow ,keyword eow (+ space) (group (regexp ,ident)))
                  t)))

(defvar swift-mode--imenu-generic-expression
  (list
   (list "Functions" (swift-mode--mk-regex-for-def "func") 1)
   (list "Classes"   (swift-mode--mk-regex-for-def "class") 1)
   (list "Enums"     (swift-mode--mk-regex-for-def "enum") 1)
   (list "Protocols" (swift-mode--mk-regex-for-def "protocol") 1)
   (list "Structs"   (swift-mode--mk-regex-for-def "struct") 1)
   (list "Constants" (swift-mode--mk-regex-for-def "let") 1)
   (list "Variables" (swift-mode--mk-regex-for-def "var") 1))
  "Value for `imenu-generic-expression' in swift-mode.")

;;; Flycheck

(eval-after-load 'flycheck
  '(progn
     (flycheck-def-option-var flycheck-swift-sdk-path nil swift
       "A path to the targeted SDK"
       :type '(repeat (directory :tag "iOS/MacOS SDK directory"))
       :safe #'flycheck-string-list-p)

     (flycheck-def-option-var flycheck-swift-linked-sources nil swift
       "Source files path to link against. Can be glob, i.e. *.swift"
       :type '(choice (const :tag "Don't use linked sources" nil)
                      (string :tag "Linked Sources"))
       :safe #'stringp)

     (flycheck-define-checker swift
       "Flycheck plugin for for Apple's Swift programming language."
       :command ("swift"
                 "-frontend" "-parse"
                 (option-list "-sdk" flycheck-swift-sdk-path)
                 ;; Swift compiler will complain about redeclaration
                 ;; if we will include original file along with
                 ;; temporary source file created by flycheck.
                 ;; We also don't want a hidden emacs interlock files.
                 (eval
                  (let (source file)
                    (setq source (flycheck-substitute-argument 'source 'swift))
                    (setq file (file-name-nondirectory source))
                    (remove-if-not
                     #'(lambda (path)
                         (and
                          (eq (string-match ".#" path) nil)
                          (eq (string-match file path) nil)))
                     (file-expand-wildcards flycheck-swift-linked-sources))))
                 "-primary-file" source)
       :error-patterns
       ((error line-start (file-name) ":" line ":" column ": "
               "error: " (message) line-end)
        (warning line-start (file-name) ":" line ":" column ": "
                 "warning: " (message) line-end))
       :modes swift-mode)

     (add-to-list 'flycheck-checkers 'swift)))

;;; REPL

(defvar swift-repl-buffer nil
  "Stores the name of the current swift REPL buffer, or nil.")

;;;###autoload
(defun swift-mode-run-repl (cmd &optional dont-switch-p)
  "Run a REPL process, input and output via buffer `*swift-repl*'.
If there is a process already running in `*swift-repl*', switch to that buffer.
With argument CMD allows you to edit the command line (default is value
of `swift-repl-executable').
With DONT-SWITCH-P cursor will stay in current buffer.
Runs the hook `swift-repl-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
                         (read-string "Run swift REPL: " swift-repl-executable)
                       swift-repl-executable)))
  (unless (comint-check-proc "*swift-repl*")
    (save-excursion (let ((cmdlist (split-string cmd)))
                      (set-buffer (apply 'make-comint "swift-repl" (car cmdlist)
                                         nil (cdr cmdlist)))
                      (swift-repl-mode))))
  (setq swift-repl-executable cmd)
  (setq swift-repl-buffer "*swift-repl*")
  (unless dont-switch-p
    (pop-to-buffer "*swift-repl*")))

(defun swift-mode-send-region (start end)
  "Send the current region to the inferior swift process.
START and END define region within current buffer"
  (interactive "r")
  (swift-mode-run-repl swift-repl-executable t)
  (comint-send-region swift-repl-buffer start end)
  (comint-send-string swift-repl-buffer "\n"))

(defun swift-mode-send-buffer ()
  "Send the buffer to the Swift REPL process."
  (interactive)
  (swift-mode-send-region (point-min) (point-max)))

(define-derived-mode swift-repl-mode comint-mode "Swift REPL"
  "Major mode for interacting with Swift REPL.

A REPL can be fired up with M-x swift-mode-run-repl.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
swift-repl-mode-hook (in that order).

You can send text to the REPL process from other buffers containing source.
    swift-mode-send-region sends the current region to the REPL process,
    swift-mode-send-buffer sends the current buffer to the REPL process.
")

;;; Mode definition

(defvar swift-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Additional symbols
    (modify-syntax-entry ?_ "-" table)
    (modify-syntax-entry ?: "_" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)

    ;; Parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    table))

(defvar swift-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") 'swift-mode-run-repl)
    (define-key map (kbd "C-c C-f") 'swift-mode-send-buffer)
    (define-key map (kbd "C-c C-r") 'swift-mode-send-region)
    (easy-menu-define swift-menu map "Swift Mode menu"
      `("Swift"
        :help "Swift-specific Features"
        ["Run REPL" swift-mode-run-repl
         :help "Run Swift REPL"]
        ["Send buffer to REPL" swift-mode-send-buffer
         :help "Send the current buffer's contents to the REPL"]
        ["Send region to REPL" swift-mode-send-region
         :help "Send currently selected region to the REPL"]))
    map)
  "Key map for swift mode.")

;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for Apple's Swift programming language.

\\<swift-mode-map>"
  :group 'swift
  :syntax-table swift-mode-syntax-table
  (setq font-lock-defaults '((swift-font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'swift-syntax-propertize-function)

  (setq-local imenu-generic-expression swift-mode--imenu-generic-expression)

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local tab-width swift-indent-offset)
  (setq-local indent-tabs-mode nil)
  (setq-local electric-indent-chars
              (append '(?. ?, ?: ?\) ?\] ?\}) electric-indent-chars))
  (smie-setup swift-smie-grammar 'swift-smie-rules ;; 'verbose-swift-smie-rules
              :forward-token 'swift-smie--forward-token
              :backward-token 'swift-smie--backward-token))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

(provide 'swift-mode)

;;; swift-mode.el ends here
