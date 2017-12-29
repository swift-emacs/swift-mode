;;; swift-mode-font-lock.el --- Major-mode for Apple's Swift programming language, Font Locks. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;
;; Version: 4.0.1
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

(defun swift-mode:function-name-pos-p (pos)
  "Return t if POS is at just before function name."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (forward-comment (- (point)))
      (skip-syntax-backward "w_")
      (looking-at "\\<\\(func\\|enum\\|struct\\|class\\|protocol\\|extension\\)\\>"))))

(defun swift-mode:font-lock-match-function-names (limit)
  "Move the cursor just after a function name or others.

Others includes enum, struct, class, protocol name.
Set `match-data', and return t if a function name found before position LIMIT.
Return nil otherwise."
  (and
   (< (point) limit)
   (re-search-forward "\\<\\(\\sw\\|\\s_\\)+\\>" limit t)
   (or
    (swift-mode:function-name-pos-p (match-beginning 0))
    (swift-mode:font-lock-match-function-names limit))))

(defconst swift-mode:font-lock-keywords
  '(
    ;; Attributes
    "@\\(\\sw\\|\\s_\\)*"

    ;; Constants
    ("\\<true\\>" . font-lock-constant-face)
    ("\\<false\\>" . font-lock-constant-face)
    ("\\<nil\\>" . font-lock-constant-face)

    ;; Keywords
    ;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410

    ;; Keywords that begin with a number sign (#)
    ("#available\\>" . font-lock-preprocessor-face)
    ("#colorLiteral\\>" . font-lock-preprocessor-face)
    ("#column\\>" . font-lock-preprocessor-face)
    ("#else\\>" . font-lock-preprocessor-face)
    ("#elseif\\>" . font-lock-preprocessor-face)
    ("#endif\\>" . font-lock-preprocessor-face)
    ("#fileLiteral\\>" . font-lock-preprocessor-face)
    ("#file\\>" . font-lock-preprocessor-face)
    ("#function\\>" . font-lock-preprocessor-face)
    ("#if\\>" . font-lock-preprocessor-face)
    ("#imageLiteral\\>" . font-lock-preprocessor-face)
    ("#keypath\\>" . font-lock-preprocessor-face)
    ("#line\\>" . font-lock-preprocessor-face)
    ("#selector\\>" . font-lock-preprocessor-face)
    ("#sourceLocation\\>" . font-lock-preprocessor-face)

    ;; Keywords used in declarations
    "\\<associatedtype\\>"
    "\\<class\\>"
    "\\<deinit\\>"
    "\\<enum\\>"
    "\\<extension\\>"
    "\\<fileprivate\\>"
    "\\<func\\>"
    "\\<import\\>"
    "\\<init\\>"
    "\\<inout\\>"
    "\\<internal\\>"
    "\\<let\\>"
    "\\<open\\>"
    "\\<operator\\>"
    "\\<private\\>"
    "\\<protocol\\>"
    "\\<public\\>"
    "\\<static\\>"
    "\\<struct\\>"
    "\\<subscript\\>"
    "\\<typealias\\>"
    "\\<var\\>"

    ;; Keywords used in statements
    "\\<break\\>"
    "\\<case\\>"
    "\\<continue\\>"
    "\\<default\\>"
    "\\<defer\\>"
    "\\<do\\>"
    "\\<else\\>"
    "\\<fallthrough\\>"
    "\\<for\\>"
    "\\<guard\\>"
    "\\<if\\>"
    "\\<in\\>"
    "\\<repeat\\>"
    "\\<return\\>"
    "\\<switch\\>"
    "\\<where\\>"
    "\\<while\\>"

    ;; Keywords used in expressions and types (without true, false, and keywords begin with a number sign)
    "\\<as\\>"
    "\\<catch\\>"
    "\\<dynamicType\\>"
    "\\<is\\>"
    "\\<rethrows\\>"
    "\\<super\\>"
    "\\<self\\>"
    "\\<Self\\>"
    "\\<throws\\>"
    "\\<throw\\>"
    "\\<try\\>"

    ;; Keywords reserved in particular contexts
    "\\<Protocol\\>"
    "\\<Type\\>"
    "\\<and\\>"
    "\\<assignment\\>"
    "\\<associativity\\>"
    "\\<convenience\\>"
    "\\<didSet\\>"
    "\\<dynamic\\>"
    "\\<final\\>"
    "\\<get\\>"
    "\\<higherThan\\>"
    "\\<indirect\\>"
    "\\<infix\\>"
    "\\<lazy\\>"
    "\\<left\\>"
    "\\<lowerThan\\>"
    "\\<mutating\\>"
    "\\<none\\>"
    "\\<nonmutating\\>"
    "\\<optional\\>"
    "\\<override\\>"
    "\\<postfix\\>"
    "\\<precedence\\>"
    "\\<precedencegroup\\>"
    "\\<prefix\\>"
    "\\<required\\>"
    "\\<right\\>"
    "\\<set\\>"
    "\\<unowned\\>"
    "\\<weak\\>"
    "\\<willSet\\>"

    ;; Standard library functions
    ;; https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_StandardLibrary_Functions/index.html#//apple_ref/doc/uid/TP40016052
    ("\\<abs\\>" . font-lock-builtin-face)
    ("\\<alignof\\>" . font-lock-builtin-face)
    ("\\<alignofValue\\>" . font-lock-builtin-face)
    ("\\<anyGenerator\\>" . font-lock-builtin-face)
    ("\\<assert\\>" . font-lock-builtin-face)
    ("\\<assertionFailure\\>" . font-lock-builtin-face)
    ("\\<debugPrint\\>" . font-lock-builtin-face)
    ("\\<dump\\>" . font-lock-builtin-face)
    ("\\<fatalError\\>" . font-lock-builtin-face)
    ("\\<getVaList\\>" . font-lock-builtin-face)
    ("\\<isUniquelyReferenced\\>" . font-lock-builtin-face)
    ("\\<isUniquelyReferencedNonObjC\\>" . font-lock-builtin-face)
    ("\\<max\\>" . font-lock-builtin-face)
    ("\\<min\\>" . font-lock-builtin-face)
    ("\\<numericCast\\>" . font-lock-builtin-face)
    ("\\<precondition\\>" . font-lock-builtin-face)
    ("\\<preconditionFailure\\>" . font-lock-builtin-face)
    ("\\<print\\>" . font-lock-builtin-face)
    ("\\<readLine\\>" . font-lock-builtin-face)
    ("\\<sizeof\\>" . font-lock-builtin-face)
    ("\\<sizeofValue\\>" . font-lock-builtin-face)
    ("\\<strideof\\>" . font-lock-builtin-face)
    ("\\<strideofValue\\>" . font-lock-builtin-face)
    ("\\<swap\\>" . font-lock-builtin-face)
    ("\\<transcode\\>" . font-lock-builtin-face)
    ("\\<unsafeAddressOf\\>" . font-lock-builtin-face)
    ("\\<unsafeBitCast\\>" . font-lock-builtin-face)
    ("\\<unsafeDowncast\\>" . font-lock-builtin-face)
    ("\\<unsafeUnwrap\\>" . font-lock-builtin-face)
    ("\\<withExtendedLifetime\\>" . font-lock-builtin-face)
    ("\\<withUnsafeMutablePointer\\>" . font-lock-builtin-face)
    ("\\<withUnsafeMutablePointers\\>" . font-lock-builtin-face)
    ("\\<withUnsafePointer\\>" . font-lock-builtin-face)
    ("\\<withUnsafePointers\\>" . font-lock-builtin-face)
    ("\\<withVaList\\>" . font-lock-builtin-face)
    ("\\<zip\\>" . font-lock-builtin-face)

    ;; keywords for build configuration statements
    ("\\<os\\>" . font-lock-builtin-face)
    ("\\<arch\\>" . font-lock-builtin-face)
    ("\\<swift\\>" . font-lock-builtin-face)
    ("\\<OSX\\>" . font-lock-builtin-face)
    ("\\<iOS\\>" . font-lock-builtin-face)
    ("\\<watchOS\\>" . font-lock-builtin-face)
    ("\\<tvOS\\>" . font-lock-builtin-face)
    ("\\<i386\\>" . font-lock-builtin-face)
    ("\\<x86_64\\>" . font-lock-builtin-face)
    ("\\<arm\\>" . font-lock-builtin-face)
    ("\\<arm64\\>" . font-lock-builtin-face)
    ("\\<iOSApplicationExtension\\>" . font-lock-builtin-face)
    ("\\<OSXApplicationExtension\\>" . font-lock-builtin-face)

    (swift-mode:font-lock-match-function-names . font-lock-function-name-face)
    )
  "Swift mode keywords for Font Lock.")


(provide 'swift-mode-font-lock)

;;; swift-mode-font-lock.el ends here
