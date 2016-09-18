// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)


// Simple case

let x = 1 +
  2 +
  3 + 4 +
  5

let x = 1
  + 2
  + 3 + 4
  + 5

// User defined operator

let x = 1 +++
  2 /=-+!*%<>&|^?~
  3 +++ 4 +++
  5

// Prefix operators and postfix operators

let x = 1
+++foo()

let x = 1+++
foo()

// Comments precede over operators

let x = 1 +// abc
  2 +/* abc*/
  3

// Comments behave like whitespaces
// https://github.com/apple/swift-evolution/blob/master/proposals/0037-clarify-comments-and-operators.md
let x = 1
/*a*/+++foo()

let x = 1
  /*a*/+++ foo()

let x = 1+++//
foo()

let x = 1+++/*a*/
foo()

let x = 1 +++/*a*/
  foo()

// Operators with dot

// This must be equal to let x = (a.++.) a
let x = a.++.
b

// This must be equal to let x = (a++) . a
let x = a++.
  a

// Unicode operators

let x = a ×
  a  // swift-mode:test:known-bug
