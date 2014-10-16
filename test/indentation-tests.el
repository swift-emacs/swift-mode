;;; indentation-tests.el --- Test swift-mode indentation behaviour

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

;; Test swift-mode indentation behaviour

;;; Code:

(require 'ert)
(require 'swift-mode)
(require 's)

;;; Test utilities

(defmacro check-indentation (description before after &optional var-bindings)
  "Declare an ert test for indentation behaviour.
The test will check that the swift indentation command changes the buffer
from one state to another.  It will also test that point is moved to an
expected position.

DESCRIPTION is a symbol describing the test.

BEFORE is the buffer string before indenting, where a pipe (|) represents
point.

AFTER is the expected buffer string after indenting, where a pipe (|)
represents the expected position of point.

VAR-BINDINGS is an optional let-bindings list.  It can be used to set the
values of customisable variables."
  (declare (indent 1))
  (let ((fname (intern (format "indentation/%s" description))))
    `(ert-deftest ,fname ()
       (let* ((after ,after)
              (expected-cursor-pos (1+ (s-index-of "|" after)))
              (expected-state (delete ?| after))

              ;; Bind customisable vars to default values for tests.
              (swift-indent-offset 4)
              (swift-indent-switch-case-offset 0)
              (swift-indent-multiline-statement-offset 2)
              ,@var-bindings)
         (with-temp-buffer
           (insert ,before)
           (goto-char (point-min))
           (search-forward "|")
           (delete-char -1)
           (swift-mode)
           (indent-according-to-mode)

           (should (equal expected-state (buffer-string)))
           (should (equal expected-cursor-pos (point))))))))

;; Provide font locking for easier test editing.

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group "check-indentation") eow)
    (1 font-lock-keyword-face))
   (,(rx "("
         (group "check-indentation") (+ space)
         (group bow (+ (not space)) eow)
         )
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))


;;; Tests


(check-indentation no-indentation-at-top-level
  "|x"
  "|x")

(check-indentation toplevel-exprs-indented-to-same-level/1
  "
x
|y
" "
x
|y
")

(check-indentation toplevel-exprs-indented-to-same-level/2
  "
x
     |y
" "
x
|y
")

(check-indentation nested-exprs-indented-to-same-level/1
  "
{
    x
    |y
}
" "
{
    x
    |y
}
")

(check-indentation nested-exprs-indented-to-same-level/2
  "
{
    x
        |y
}
" "
{
    x
    |y
}
")

(check-indentation nested-exprs-indented-to-same-level/3
  "
{
    x
|y
}
" "
{
    x
    |y
}
")

(check-indentation indent-if-body
  "
if true {
|x
}
" "
if true {
    |x
}
")

(check-indentation indent-if-else
  "
if true {
} else {
|foo
}
" "
if true {
} else {
    |foo
}
")

(check-indentation indent-if-else-if
  "
if true {
} else if false {
|foo
}
" "
if true {
} else if false {
    |foo
}
")

(check-indentation indent-if-body--no-effect-if-already-indented
  "
if true {
    |x
}
""
if true {
    |x
}
")

(check-indentation indent-if-body-nested
  "
if foo {
    if true {
|foo
    }
}
" "
if foo {
    if true {
        |foo
    }
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/1
  "
switch true {
    |case
}
" "
switch true {
|case
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/2
  "
switch true {
          |case
}
" "
switch true {
|case
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/3
  "
{
    switch true {
|case
    }
}
" "
{
    switch true {
    |case
    }
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/4
  "
{
    switch true {
              |case
    }
}
" "
{
    switch true {
    |case
    }
}
")


(check-indentation indents-case-statement-bodies/1
"
switch x {
case y:
|return z
}
" "
switch x {
case y:
    |return z
}
")

(check-indentation indents-case-statement-bodies/2
"
switch x {
case y:
       |return z
}
" "
switch x {
case y:
    |return z
}
")

(check-indentation indents-case-statement-bodies/3
"
switch x {
case y:
    |return z
}
" "
switch x {
case y:
    |return z
}
")

(check-indentation indents-case-statement-bodies/4
"
switch x {
case y:
    x
    |return z
}
" "
switch x {
case y:
    x
    |return z
}
")

(check-indentation indents-case-statement-bodies/5
"
switch x {
case y:
    x
|return z
}
" "
switch x {
case y:
    x
    |return z
}
")

(check-indentation indents-case-statement-bodies/6
"
switch x {
case y:
    x
        |return z
}
" "
switch x {
case y:
    x
    |return z
}
")


(check-indentation indents-default-statements-to-same-level-as-enclosing-switch/1
  "
{
    switch true {
|default
    }
}
" "
{
    switch true {
    |default
    }
}
")

(check-indentation indents-default-statements-to-same-level-as-enclosing-switch/2
  "
{
    switch true {
              |default
    }
}
" "
{
    switch true {
    |default
    }
}
")

(check-indentation indents-case-statements-with-destucturing/1
  "
switch true {
case let(x, y):
|foo
}
" "
switch true {
case let(x, y):
    |foo
}
")

(check-indentation indents-case-statements-with-destucturing/2
  "
switch true {
case let .Foo(x):
|foo
}
" "
switch true {
case let .Foo(x):
    |foo
}
")

(check-indentation indents-case-statements-with-guard
  "
switch true {
case foo where bar:
|foo
}
" "
switch true {
case foo where bar:
    |foo
}
")

(check-indentation indents-case-statements-with-multiline-guard/1
  "
switch true {
case foo where bar,
|bar where baz:
}
" "
switch true {
case foo where bar,
     |bar where baz:
}
")

(check-indentation indents-case-statements-with-multiline-guard/2
  "
switch true {
case foo where bar,
     bar where baz:
|foo
}
" "
switch true {
case foo where bar,
     bar where baz:
    |foo
}
")


(check-indentation indents-case-statements-to-user-defined-offset/1
  "
switch true {
    |case
}
" "
switch true {
  |case
}
"
((swift-indent-switch-case-offset 2)))

(check-indentation indents-case-statements-to-user-defined-offset/2
  "
switch true {
          |case
}
" "
switch true {
  |case
}
"
((swift-indent-switch-case-offset 2)))


(check-indentation indents-case-statements-in-enum/1
  "
enum T {
|case
}
" "
enum T {
    |case
}
")

(check-indentation indents-case-statements-in-enum/2
  "
enum T {
         |case
}
" "
enum T {
    |case
}
")

(check-indentation indents-case-statements-in-enum/3
  "
enum Foo: Bar {
         |case
}
" "
enum Foo: Bar {
    |case
}
")

(check-indentation indents-declaration-statements-in-enum/1
                   "
enum Foo: Bar {
    case foo
    case bar
         |var foo
}
" "
enum Foo: Bar {
    case foo
    case bar
    |var foo
}
")

(check-indentation indents-for-statements/1
  "
for index in 1..5 {
|foo
}
" "
for index in 1..5 {
    |foo
}
")

(check-indentation indents-for-statements/2
  "
for (key, value) in dict {
|foo
}
" "
for (key, value) in dict {
    |foo
}
")

(check-indentation indents-for-statements/3
  "
for var index = 0; index < 3; ++index  {
|foo
}
" "
for var index = 0; index < 3; ++index  {
    |foo
}
")

(check-indentation indents-while-statements
  "
while foo < bar{
|foo
}
" "
while foo < bar{
    |foo
}
")

(check-indentation indents-import-statements/1
  "
import Foo
        |import Bar
" "
import Foo
|import Bar
")

(check-indentation indents-class-declaration/1
  "
class Foo {
          |foo
}
" "
class Foo {
    |foo
}
")

(check-indentation indents-class-declaration/2
  "
class Foo: Bar {
          |foo
}
" "
class Foo: Bar {
    |foo
}
")

(check-indentation indents-class-declaration/3
  "
class Foo: Foo, Bar, Baz {
          |foo
}
" "
class Foo: Foo, Bar, Baz {
    |foo
}
")

(check-indentation indents-class-declaration/4
  "
class Foo: Bar {
|class Baz: Bar {
    }
}
" "
class Foo: Bar {
    |class Baz: Bar {
    }
}
")

(check-indentation indents-func-declaration/1
  "
func Foo(a: String) {
|foo
}
" "
func Foo(a: String) {
    |foo
}
")

(check-indentation indents-func-declaration/2
  "
override func Foo() {
|foo
}
" "
override func Foo() {
    |foo
}
")

(check-indentation indents-func-declaration/3
  "
func Foo(b: Double...) -> Bool {
|foo
}
" "
func Foo(b: Double...) -> Bool {
    |foo
}
")

(check-indentation indents-func-declaration/4
  "
class Foo {
    override func Foo(b: Double...) -> Bool {
|foo
    }
}
" "
class Foo {
    override func Foo(b: Double...) -> Bool {
        |foo
    }
}
")

(check-indentation indents-func-declaration/2
                   "
class func Foo() {
|foo
}
" "
class func Foo() {
    |foo
}
")

(check-indentation indents-declaration/1
  "
var foo = bar + baz
          |
" "
var foo = bar + baz
|
")

(check-indentation indents-declaration/2
  "
let foo = bar +
|baz
" "
let foo = bar +
          |baz
")

(check-indentation indents-declaration/3
  "
let foo = [foo: bar, bar: baz]
          |
" "
let foo = [foo: bar, bar: baz]
|
")

(check-indentation indents-declaration/4
  "
let foo = [
|bar: baz
]
" "
let foo = [
    |bar: baz
]
")

(check-indentation indents-declaration/5
  "
let foo = [foo, bar]
          |
" "
let foo = [foo, bar]
|
")

(check-indentation indents-declaration/6
  "
let foo = [
|bar
]
" "
let foo = [
    |bar
]
")

(check-indentation indents-declaration/7
                   "
var result = Dictionary<String, V>()
    |foo
" "
var result = Dictionary<String, V>()
|foo
")

(check-indentation indents-multiline-expressions/1
"
Foo.bar([foo: bar,
|bar: baz
])
" "
Foo.bar([foo: bar,
         |bar: baz
])
")

(check-indentation indents-multiline-expressions/2
                   "
Foo.bar(bar!,
|baz)
" "
Foo.bar(bar!,
        |baz)
")

(check-indentation indents-multiline-expressions/3
                   "
Foo.bar(bar?,
|baz)
" "
Foo.bar(bar?,
        |baz)
")

(check-indentation indents-multiline-expressions/4
                   "
let json_ary = NSJSONSerialization.
|JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
" "
let json_ary = NSJSONSerialization.
               |JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
")

(check-indentation indents-multiline-expressions/5
                   "
let json_ary = NSJSONSerialization
|.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
" "
let json_ary = NSJSONSerialization
               |.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
")

(check-indentation indents-multiline-expressions/6
                   "
let json_ary = NSJSONSerialization.
               |JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
" "
let json_ary = NSJSONSerialization.
               |JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
")

(check-indentation indents-multiline-expressions/7
                   "
let json_ary = NSJSONSerialization
               |.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
" "
let json_ary = NSJSONSerialization
               |.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
")

(check-indentation indents-multiline-expressions/8
                   "
let options = NSRegularExpressionOptions.CaseInsensitive &
|NSRegularExpressionOptions.DotMatchesLineSeparators
" "
let options = NSRegularExpressionOptions.CaseInsensitive &
              |NSRegularExpressionOptions.DotMatchesLineSeparators
")

(check-indentation indents-multiline-expressions-to-user-defined-offset/1
                   "
NSNotificationCenter.defaultCenter().
|postNotificationName(foo, object: nil)
" "
NSNotificationCenter.defaultCenter().
    |postNotificationName(foo, object: nil)
"
((swift-indent-multiline-statement-offset 4)))

(check-indentation indents-multiline-expressions-to-user-defined-offset/2
                   "
NSNotificationCenter.defaultCenter()
|.postNotificationName(foo, object: nil)
" "
NSNotificationCenter.defaultCenter()
    |.postNotificationName(foo, object: nil)
"
((swift-indent-multiline-statement-offset 4)))

(check-indentation indents-multiline-expressions-to-user-defined-offset/3
                   "
let json_ary = NSJSONSerialization
               |.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
" "
let json_ary = NSJSONSerialization
                 |.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
"
((swift-indent-multiline-statement-offset 4)))

(check-indentation indents-multiline-expressions-to-user-defined-offset/4
                   "
let options = NSRegularExpressionOptions.CaseInsensitive &
|NSRegularExpressionOptions.DotMatchesLineSeparators
" "
let options = NSRegularExpressionOptions.CaseInsensitive &
                |NSRegularExpressionOptions.DotMatchesLineSeparators
"
((swift-indent-multiline-statement-offset 4)))

(check-indentation indents-type-annotations/1
                   "
typealias Foo = Bar<Foo.Baz, Foo>
    |foo
" "
typealias Foo = Bar<Foo.Baz, Foo>
|foo
")

(check-indentation indents-type-annotations/2
                   "
typealias Foo = Bar<Foo.Baz,
|Foo>
" "
typealias Foo = Bar<Foo.Baz,
                    |Foo>
")

(check-indentation indents-type-works-with-less-operator/1
                   "
typealias Foo = Bar<Foo.Baz, Foo>
let foo = bar <
|baz
" "
typealias Foo = Bar<Foo.Baz, Foo>
let foo = bar <
          |baz
")

(check-indentation indents-type-works-with-less-operator/2
                   "
typealias Foo = Bar<Foo.Baz, Foo>
let foo = bar >
|baz
" "
typealias Foo = Bar<Foo.Baz, Foo>
let foo = bar >
          |baz
")

(provide 'indentation-tests)

;;; indentation-tests.el ends here
