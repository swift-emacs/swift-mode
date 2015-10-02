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
              (swift-indent-hanging-comma-offset nil)
              ,@var-bindings)
         (with-temp-buffer
           (insert ,before)
           (goto-char (point-min))
           (search-forward "|")
           (delete-char -1)
           (swift-mode)
           (setq smie-forward-token-function 'swift-smie--forward-token-debug)
           (setq smie-backward-token-function 'swift-smie--backward-token-debug)
           (indent-according-to-mode)

           (should (equal expected-state (buffer-string)))
           (should (equal expected-cursor-pos (point)))

           (goto-char (point-min))
           (forward-sexp 10)
           (should (equal (point-max) (point)))
           (forward-sexp -10)
           (should (equal (point-min) (point)))

           (goto-char (point-min))
           (forward-list 10)
           (should (equal (point-max) (point)))
           (forward-list -10)
           (should (equal (point-min) (point)))
           )))))

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
    |case foo:
}
" "
switch true {
|case foo:
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/2
  "
switch true {
          |case foo:
}
" "
switch true {
|case foo:
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/3
  "
{
    switch true {
|case foo:
    }
}
" "
{
    switch true {
    |case foo:
    }
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/4
  "
{
    switch true {
              |case foo:
    }
}
" "
{
    switch true {
    |case foo:
    }
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/5
                   "
switch {
    |case foo,
     bar, buz:
    foo
}
" "
switch {
|case foo,
     bar, buz:
    foo
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/6
                   "
switch {
    |case
    foo, bar, buz:
    foo
}
" "
switch {
|case
    foo, bar, buz:
    foo
}
")

(check-indentation indents-case-statements-to-same-level-as-enclosing-switch/7
                   "
switch {
case foo:
    foo
    bar
  |case baz:
}
" "
switch {
case foo:
    foo
    bar
|case baz:
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
    case y:
        x
|default:
    }
}
" "
{
    switch true {
    case y:
        x
    |default:
    }
}
")

(check-indentation indents-default-statements-to-same-level-as-enclosing-switch/2
  "
{
    switch true {
    case y:
        x
              |default:
    }
}
" "
{
    switch true {
    case y:
        x
    |default:
    }
}
")

(check-indentation indents-default-statements-to-same-level-as-enclosing-switch/3
                   "
{
    switch true {
    case y:
        x
    default:
        foo
        |}
}
" "
{
    switch true {
    case y:
        x
    default:
        foo
    |}
}
")

(check-indentation indents-statements-under-default-case/1
  "
{
    switch true {
    case y:
        x
    default:
    |z
    }
}
" "
{
    switch true {
    case y:
        x
    default:
        |z
    }
}
")

(check-indentation indents-case-statements-with-destucturing/1
  "
switch true {
case let(x, y) where x < y:
|foo
}
" "
switch true {
case let(x, y) where x < y:
    |foo
}
")

(check-indentation indents-case-statements-with-destucturing/2
  "
switch true {
case let .Foo(x) where x > 0:
|foo
}
" "
switch true {
case let .Foo(x) where x > 0:
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

(check-indentation indents-case-statements-with-multiline-guard-custom-offset/1
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
"
((swift-indent-hanging-comma-offset 3)))

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
    |case foo:
}
" "
switch true {
  |case foo:
}
"
((swift-indent-switch-case-offset 2)))

(check-indentation indents-case-statements-to-user-defined-offset/2
  "
switch true {
          |default:
}
" "
switch true {
  |default:
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

(check-indentation indents-import-statements/2
  "
import Darwin
    |class Test {
}
" "
import Darwin
|class Test {
}
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

(check-indentation indents-class-declaration/5
                   "
class Foo: Foo, Bar,
|Baz {
}
" "
class Foo: Foo, Bar,
      |Baz {
}
")

(check-indentation indents-class-declaration-custom-offset/1
                   "
class Foo: Foo, Bar,
|Baz {
}
" "
class Foo: Foo, Bar,
   |Baz {
}
"
((swift-indent-hanging-comma-offset 3)))

(check-indentation indents-class-declaration/6
                   "
class Foo:
|Foo, Bar, Baz {
}
" "
class Foo:
    |Foo, Bar, Baz {
}
")

(check-indentation indents-class-declaration/7
                   "
class Foo: Bar<A, B,
|C>
" "
class Foo: Bar<A, B,
               |C>
")

(check-indentation indents-class-declaration/8
                   "
class Foo<A: B<C>>:
                   |Bar
" "
class Foo<A: B<C>>:
    |Bar
")

(check-indentation indents-class-declaration/9
                   "
class Foo: Foo,
      Bar,
      Bar2,
         |Baz {
}
" "
class Foo: Foo,
      Bar,
      Bar2,
      |Baz {
}
")

(check-indentation indents-class-declaration/10
                   "
class Foo: Foo,
      Bar,
      Bar2,
      Baz {
  |}
" "
class Foo: Foo,
      Bar,
      Bar2,
      Baz {
|}
")

(check-indentation indents-public-class-declaration/1
                   "
public class Foo: Foo, Bar,
|Baz {
}
" "
public class Foo: Foo, Bar,
             |Baz {
}
")

(check-indentation indents-public-class-declaration-custom-offset/1
                   "
public class Foo: Foo, Bar,
|Baz {
}
" "
public class Foo: Foo, Bar,
   |Baz {
}
"
((swift-indent-hanging-comma-offset 3)))

(check-indentation indents-public-class-declaration/2
  "
public class Foo {
          |foo
}
" "
public class Foo {
    |foo
}
")

(check-indentation indents-public-class-declaration/3
                   "
public class Foo: Foo, Bar,
             Baz {
  |}
" "
public class Foo: Foo, Bar,
             Baz {
|}
")

(check-indentation indents-public-class-declaration/4
                   "
public class Foo: Foo, Bar,
             Baz {
|foo
}
" "
public class Foo: Foo, Bar,
             Baz {
    |foo
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

(check-indentation indents-func-declaration/5
                   "
class func Foo() {
|foo
}
" "
class func Foo() {
    |foo
}
")

(check-indentation indents-func-declaration/6
                   "
func Foo(aaaaaaaaa:
         |AAAAAAAAA) {
}
" "
func Foo(aaaaaaaaa:
         |AAAAAAAAA) {
}
")

(check-indentation indents-func-declaration/7
  "
func foo() ->
|Foo
" "
func foo() ->
    |Foo
")

(check-indentation indents-func-declaration/8
  "
func foo() ->
|(A, B) {}
" "
func foo() ->
    |(A, B) {}
")

(check-indentation indents-func-declaration/9
  "
func foo() ->
|[A] {}
" "
func foo() ->
    |[A] {}
")

(check-indentation indents-func-declaration/10
                   "
func a(a: NSString = 1,
                     |b: NSString = 2) {}
" "
func a(a: NSString = 1,
       |b: NSString = 2) {}
")

(check-indentation indents-func-declaration/11
                   "
class Foo: Bar {
    func Foo() {
|foo
    }
}
" "
class Foo: Bar {
    func Foo() {
        |foo
    }
}
")

(check-indentation indents-func-declaration/12
                   "
class Foo: Bar {
    override func Foo() {
|foo
    }
}
" "
class Foo: Bar {
    override func Foo() {
        |foo
    }
}
")

(check-indentation indents-protocol-declaration/1
                   "
protocol Foo {
    func foo()
|func bar()
}
" "
protocol Foo {
    func foo()
    |func bar()
}
")

(check-indentation indents-protocol-declaration/2
                   "
protocol Foo {
    func foo() -> Foo
|func bar() -> Bar
}
" "
protocol Foo {
    func foo() -> Foo
    |func bar() -> Bar
}
")

(check-indentation indents-protocol-declaration/3
                   "
protocol Foo {
    func foo() -> Foo<A>
|func bar() -> Bar<A>
}
" "
protocol Foo {
    func foo() -> Foo<A>
    |func bar() -> Bar<A>
}
")

(check-indentation indents-protocol-declaration/4
                   "
protocol Foo {
    func foo() -> [A]
|func bar() -> [A]
}
" "
protocol Foo {
    func foo() -> [A]
    |func bar() -> [A]
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
(check-indentation indents-declaration/8
  "
let foo =
|bar
" "
let foo =
    |bar
")

(check-indentation indents-declaration/9
  "
let foo: Foo? =
|bar
" "
let foo: Foo? =
    |bar
")

(check-indentation indents-declaration/10
  "
let foo: Foo<A> =
|bar
" "
let foo: Foo<A> =
    |bar
")

(check-indentation indents-declaration/11
  "
let foo = [
    foo:
|bar
]
" "
let foo = [
    foo:
    |bar
]
")

(check-indentation indents-declaration/12
  "
let foo = [
|[]]
" "
let foo = [
    |[]]
")

(check-indentation indents-declaration/13
                   "
let foo = [
|[
        bar: baz
    ]
]
" "
let foo = [
    |[
        bar: baz
    ]
]
")

(check-indentation indents-declaration/14
                   "
let foo = [
    [
    |bar: baz
    ]
]
" "
let foo = [
    [
        |bar: baz
    ]
]
")

(check-indentation indents-declaration/15
                   "
let foo = [
    [
        bar: baz
|]
]
" "
let foo = [
    [
        bar: baz
    |]
]
")

(check-indentation indents-expressions/1
                   "
class Foo {
    func a() {
    |[a]
    }
}
" "
class Foo {
    func a() {
        |[a]
    }
}
")

(check-indentation indents-expressions/2
                   "
class Foo {
    func a() {
        a
    |[a]
    }
}
" "
class Foo {
    func a() {
        a
        |[a]
    }
}
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

(check-indentation indents-multiline-expressions/9
                   "
foo?[bar] +
     |a
" "
foo?[bar] +
  |a
")

(check-indentation indents-multiline-expressions/10
                   "
foo?(bar) +
     |a
" "
foo?(bar) +
  |a
")

(check-indentation indents-multiline-expressions/11
                   "
func a () {
    a +
|a
}
" "
func a () {
    a +
      |a
}
")

(check-indentation indents-multiline-expressions/12
                   "
func a () {
    a
|.a()
}
" "
func a () {
    a
      |.a()
}
")

(check-indentation indents-multiline-expressions/13
                   "
if (a
|.b){}
" "
if (a
     |.b){}
")

(check-indentation indents-multiline-expressions/14
                   "
a ??
|b
" "
a ??
  |b
")

(check-indentation indents-multiline-expressions/15
                   "
a as
|b
" "
a as
  |b
")

(check-indentation indents-multiline-expressions/16
                   "
a as?
|b
" "
a as?
  |b
")

(check-indentation indents-multiline-expressions/17
                   "
a is
|b
" "
a is
  |b
")

(check-indentation indents-multiline-expressions/18
                   "
CGPoint(x: aaaaaaaaaaaaaaa.x +
|bbbbbbbbbbbbbbbb,
        y: aaaaaaaaaaaaaaa.y +
           bbbbbbbbbbbbbbbb)
" "
CGPoint(x: aaaaaaaaaaaaaaa.x +
           |bbbbbbbbbbbbbbbb,
        y: aaaaaaaaaaaaaaa.y +
           bbbbbbbbbbbbbbbb)
")

(check-indentation indents-multiline-expressions/19
                   "
let x = 1
|+ 1
" "
let x = 1
        |+ 1
")

(check-indentation indents-multiline-expressions/20
                   "
let x = foo ??
            |bar
" "
let x = foo ??
        |bar
")

(check-indentation indents-multiline-expressions/21
                   "
let foo = a +
          b +
            |c +
          d
" "
let foo = a +
          b +
          |c +
          d
")

(check-indentation indents-multiline-expressions/22
                   "
let foo = a +
          b +
          c +
              |d
" "
let foo = a +
          b +
          c +
          |d
")

(check-indentation indents-multiline-expressions/23
                   "
let x = bar
        .buz() ??
|defaultValue
" "
let x = bar
        .buz() ??
        |defaultValue
")

(check-indentation indents-multiline-expressions/24
  "
let foo =
    bar +
  |baz +
    a
" "
let foo =
    bar +
    |baz +
    a
")

(check-indentation indents-long-parameters/1
                   "
func foo() {
    timer = NSTimer.scheduledTimerWithTimeInterval(
            |1.0,
                target: self,
                selector: Selector(\"onTimer\"),
                userInfo: nil,
                repeats: true)
}
" "
func foo() {
    timer = NSTimer.scheduledTimerWithTimeInterval(
                |1.0,
                target: self,
                selector: Selector(\"onTimer\"),
                userInfo: nil,
                repeats: true)
}
")

(check-indentation indents-long-parameters/2
                   "
aaaaaa.aaaaaaaaaaaaaaaaaaaaa(
  |aaaaaaaaaaaaaaaaaaaaa
)
" "
aaaaaa.aaaaaaaaaaaaaaaaaaaaa(
    |aaaaaaaaaaaaaaaaaaaaa
)
")

(check-indentation indents-long-parameters/3
                   "
public func tableView(
|tableView: UITableView,
    commitEditingStyle editingStyle: UITableViewCellEditingStyle,
    forRowAtIndexPath indexPath: NSIndexPath) {
}
" "
public func tableView(
    |tableView: UITableView,
    commitEditingStyle editingStyle: UITableViewCellEditingStyle,
    forRowAtIndexPath indexPath: NSIndexPath) {
}
")

(check-indentation indents-long-parameters/4
                   "
func a(
        |a: a,
    a: a,
    a: a) {
}
" "
func a(
    |a: a,
    a: a,
    a: a) {
}
")

(check-indentation indents-long-parameters/5
                   "
func foo() {
    timer = NSTimer.scheduledTimerWithTimeInterval(
                1.0,
                target: self,
                selector: Selector(\"onTimer\"),
                userInfo: nil,
                repeats: true
|)
}
" "
func foo() {
    timer = NSTimer.scheduledTimerWithTimeInterval(
                1.0,
                target: self,
                selector: Selector(\"onTimer\"),
                userInfo: nil,
                repeats: true
            |)
}
")

(check-indentation indents-multiline-expressions-to-user-defined-offset/1
                   "
NSNotificationCenter.defaultCenter()
|.postNotificationName(foo, object: nil)
" "
NSNotificationCenter.defaultCenter()
    |.postNotificationName(foo, object: nil)
"
((swift-indent-multiline-statement-offset 4)))

(check-indentation indents-multiline-expressions-to-user-defined-offset/2
                   "
let json_ary = NSJSONSerialization
               |.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
" "
let json_ary = NSJSONSerialization
                 |.JSONObjectWithData(data, options: nil, error: &json_err) as NSArray
"
((swift-indent-multiline-statement-offset 4)))

(check-indentation indents-multiline-expressions-to-user-defined-offset/3
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

(check-indentation indents-multiline-operators-only-once/1
                   "
1 +
  2 + 5 *
|3
" "
1 +
  2 + 5 *
      |3
"
)

(check-indentation indents-multiline-operators-only-once/2
                   "
1 +
  2 * 5 +
|3
" "
1 +
  2 * 5 +
  |3
"
)

(check-indentation conditional-operator/1
                   "
let a = a
        |? a +
          1
        : a +
          1
" "
let a = a
        |? a +
          1
        : a +
          1
")

(check-indentation conditional-operator/2
                   "
let a = a
        ? a +
          |1
        : a +
          1
" "
let a = a
        ? a +
          |1
        : a +
          1
")

(check-indentation conditional-operator/3
                   "
let a = a
        ? a +
          1
        |: a +
          1
" "
let a = a
        ? a +
          1
        |: a +
          1
")

(check-indentation conditional-operator/4
                   "
let a = a
        ? a +
          1
        : a +
          |1
" "
let a = a
        ? a +
          1
        : a +
          |1
")

(check-indentation conditional-operator/5
                   "
let a = a ?
|a : a
" "
let a = a ?
        |a : a
")

(check-indentation conditional-operator/6
                   "
let a = a ?
      |b :
        c
" "
let a = a ?
        |b :
        c
")

(check-indentation conditional-operator/7
                   "
let a = a ?
        b :
      |c
" "
let a = a ?
        b :
        |c
")

(check-indentation conditional-operator/8
                   "
let a = a //foo
        |? a +
          1
        : a +
          1
" "
let a = a //foo
        |? a +
          1
        : a +
          1
")

(check-indentation conditional-operator/9
                   "
func foo() {
    return order!.deliver ?
         |OrderViewTableDeliveryCells.lastCellIndex.rawValue :
           OrderViewTableTakeAwayCells.lastCellIndex.rawValue
}
" "
func foo() {
    return order!.deliver ?
           |OrderViewTableDeliveryCells.lastCellIndex.rawValue :
           OrderViewTableTakeAwayCells.lastCellIndex.rawValue
}
")

(check-indentation conditional-operator/10
                   "
func foo() {
    return order!.deliver ?
           OrderViewTableDeliveryCells.lastCellIndex.rawValue :
         |OrderViewTableTakeAwayCells.lastCellIndex.rawValue
}
" "
func foo() {
    return order!.deliver ?
           OrderViewTableDeliveryCells.lastCellIndex.rawValue :
           |OrderViewTableTakeAwayCells.lastCellIndex.rawValue
}
")

(check-indentation blank-line/1
                   "
func foo() {
    let a = 1

|let b = 1
}
" "
func foo() {
    let a = 1

    |let b = 1
}
")

(check-indentation block-inside-parenthesis/3
  "
\({
|a
})
" "
\({
     |a
})
")

(check-indentation indent-long-if-else-if/1
  "
if a {
    a
} else if a {
    a
} else if a {
    |a
} else {
    a
}
" "
if a {
    a
} else if a {
    a
} else if a {
    |a
} else {
    a
}
")

(check-indentation indent-long-if-else-if/2
  "
if a {
    a
} else if a {
    a
} else if a {
    a
|} else {
    a
}
" "
if a {
    a
} else if a {
    a
} else if a {
    a
|} else {
    a
}
")

(check-indentation indent-long-if-else-if/3
  "
class Foo {
    func a() {
        if a {
            a
        } else if b {
        |a
        } else if a {
            a
        } else {
            a
        }
    }
}
" "
class Foo {
    func a() {
        if a {
            a
        } else if b {
            |a
        } else if a {
            a
        } else {
            a
        }
    }
}
")

(check-indentation indent-long-if-else-if/4
  "
class Foo {
    func a() {
        if a {
            a
        } else if b {
            a
    |} else if a {
            a
        } else {
            a
        }
    }
}
" "
class Foo {
    func a() {
        if a {
            a
        } else if b {
            a
        |} else if a {
            a
        } else {
            a
        }
    }
}
")

(check-indentation anonymous-function-as-a-argument/1
                   "
UIView.animateWithDuration(1.0,
                           animations: {
|})
" "
UIView.animateWithDuration(1.0,
                           animations: {
                           |})
")

(check-indentation anonymous-function-as-a-argument/2
                   "
UIView.animateWithDuration(
    1.0,
    animations: {
|})
" "
UIView.animateWithDuration(
    1.0,
    animations: {
    |})
")

(check-indentation anonymous-function-as-a-argument/3
                   "
func foo() {
    UIView.animateWithDuration(1.0,
                               animations: {
                                   |}
    ) {
        completed in
    }
}
" "
func foo() {
    UIView.animateWithDuration(1.0,
                               animations: {
                               |}
    ) {
        completed in
    }
}
")

(check-indentation anonymous-function-as-a-argument/4
                   "
func foo() {
    UIView.animateWithDuration(1.0,
                               animations: {
                               }
|) {
        completed in
    }
}
" "
func foo() {
    UIView.animateWithDuration(1.0,
                               animations: {
                               }
    |) {
        completed in
    }
}
")

(check-indentation anonymous-function-as-a-argument/5
                   "
foo.bar(10,
        completionHandler: { complete in
        |foo
        }
)
" "
foo.bar(10,
        completionHandler: { complete in
            |foo
        }
)
")

(check-indentation anonymous-function-as-a-argument/6
                   "
foo.bar(10,
        completionHandler: {
            complete in
        |foo
        }
)
" "
foo.bar(10,
        completionHandler: {
            complete in
            |foo
        }
)
")

(check-indentation anonymous-function-as-a-argument/7
                   "
foo.bar(10,
        completionHandler: { (
        |bar, baz) in
            foo
        }
)
" "
foo.bar(10,
        completionHandler: { (
            |bar, baz) in
            foo
        }
)
")

(check-indentation anonymous-function-as-a-argument/8
                   "
foo.bar(10,
        completionHandler: { (bar, baz) -> Void in
        |foo
        })
" "
foo.bar(10,
        completionHandler: { (bar, baz) -> Void in
            |foo
        })
")

(check-indentation anonymous-function-as-a-argument/9
                   "
foo.bar(10,
        completionHandler: { complete in

            if foo {
                bar
            } else {
            |bar
            }
        }
)
" "
foo.bar(10,
        completionHandler: { complete in

            if foo {
                bar
            } else {
                |bar
            }
        }
)
")

(check-indentation anonymous-function-as-a-argument/10
                   "
foo.bar(10,
        completionHandler: { complete in

            if foo {
                bar
            } else {
                bar
        |}
        }
)
" "
foo.bar(10,
        completionHandler: { complete in

            if foo {
                bar
            } else {
                bar
            |}
        }
)
")

(check-indentation trailing-closure/1
                   "
a(){
    (b: String, c:String) -> String in
        |println(c)
}
" "
a(){
    (b: String, c:String) -> String in
    |println(c)
}
")

(check-indentation trailing-closure/2
                   "
a(){
    b,c in
        |println(c)
}
" "
a(){
    b,c in
    |println(c)
}
")

(check-indentation indents-expression-with-optional-type/1
                   "
var object: JsonObject?
    |var object: JsonObject
" "
var object: JsonObject?
|var object: JsonObject
")

(check-indentation indents-expression-with-optional-type/2
                   "
var object: JsonObject<Foo>?
    |var object: JsonObject
" "
var object: JsonObject<Foo>?
|var object: JsonObject
")

(check-indentation indents-expression-with-implicit-unwrapped-type/1
                   "
var object: JsonObject!
    |var object: JsonObject
" "
var object: JsonObject!
|var object: JsonObject
")

(check-indentation indents-expression-with-implicit-unwrapped-type/2
                   "
var object: JsonObject<Foo>!
    |var object: JsonObject
" "
var object: JsonObject<Foo>!
|var object: JsonObject
")

(check-indentation indents-expression-with-comment/1
                   "
func foo() {
    foo() // foo
       |foo()
}
" "
func foo() {
    foo() // foo
    |foo()
}
")

(check-indentation indents-expression-with-comment/2
                   "
func foo() {
    let x = 1 // foo
    |/ 1
}
" "
func foo() {
    let x = 1 // foo
            |/ 1
}
")

(check-indentation indents-expression-with-comment/3
                   "
func foo() {
    foo()
    //
        |foo()
}
" "
func foo() {
    foo()
    //
    |foo()
}
")

(check-indentation indents-guard-statement/1
                   "
guard let x = y else {
        |return
}
" "
guard let x = y else {
    |return
}
")

(provide 'indentation-tests)

;;; indentation-tests.el ends here
