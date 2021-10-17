//
// Negation characters.
//

//
// These "!" should be identified as negation chars.
//

if !foo {} // #("if !foo {}" 0 2 (face swift-mode:keyword-face) 3 4 (face swift-mode:negation-char-face))

if !!!!!foo {} // #("if !!!!!foo {}" 0 2 (face swift-mode:keyword-face) 3 8 (face swift-mode:negation-char-face))

if !!!!!(foo) {} // #("if !!!!!(foo) {}" 0 2 (face swift-mode:keyword-face) 3 8 (face swift-mode:negation-char-face))

if (!foo) {} // #("if (!foo) {}" 0 2 (face swift-mode:keyword-face) 4 5 (face swift-mode:negation-char-face))

let x = [!a,!b] // #("let x = [!a,!b]" 0 3 (face swift-mode:keyword-face) 9 10 (face swift-mode:negation-char-face) 12 13 (face swift-mode:negation-char-face))

let x = [a:!b] // #("let x = [a:!b]" 0 3 (face swift-mode:keyword-face) 11 12 (face swift-mode:negation-char-face))

//
// These "!" should NOT be identified as negation chars.
//

try! foo // #("try! foo" 0 3 (face swift-mode:keyword-face))

foo as! Foo // #("foo as! Foo" 4 6 (face swift-mode:keyword-face))

foo != bar // "foo != bar"

foo !== bar // "foo !== bar"

a.b.c! // #("a.b.c!" 2 3 (face swift-mode:property-access-face) 4 5 (face swift-mode:property-access-face))

a.b.c_! // #("a.b.c_!" 2 3 (face swift-mode:property-access-face) 4 5 (face swift-mode:property-access-face))

a.b.aあ! // #("a.b.a\343\201\202!" 2 3 (face swift-mode:property-access-face) 4 8 (face swift-mode:property-access-face))

init! {} // #("init! {}" 0 4 (face swift-mode:keyword-face))

let foo: Foo! = bar // #("let foo: Foo! = bar" 0 3 (face swift-mode:keyword-face))

let x = foo()! // #("let x = foo()!" 0 3 (face swift-mode:keyword-face) 8 11 (face swift-mode:function-call-face))

let x = foo[0]! // #("let x = foo[0]!" 0 3 (face swift-mode:keyword-face))

// Identifiers can be quoted.
a.b.`c`! // #("a.b.`c`!" 2 3 (face swift-mode:property-access-face) 4 7 (face font-lock-string-face))

// Custom operators.
foo +!+!+!+!+ bbb // "foo +!+!+!+!+ bbb"

//
// Regression tests.
//

enum Foo: Error { .foo } // #("enum Foo: Error { .foo }" 0 4 (face swift-mode:keyword-face) 5 8 (face swift-mode:function-name-face) 10 15 (face swift-mode:builtin-type-face) 19 22 (face swift-mode:property-access-face))

func foo() { // #("func foo() {" 0 4 (face swift-mode:keyword-face) 5 8 (face swift-mode:function-name-face))
    foo() // #("    foo()" 4 7 (face swift-mode:function-call-face))
}
