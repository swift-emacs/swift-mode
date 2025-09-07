// imports
import Foo // EXPECTED: "import" swift-mode:keyword-face "Foo" swift-mode:type-face
import typealias Foo.Foo // EXPECTED: "import" swift-mode:keyword-face "typealias" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "Foo" swift-mode:type-face
import struct Foo.Foo // EXPECTED: "import" swift-mode:keyword-face "struct" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "Foo" swift-mode:type-face
import class Foo.Foo // EXPECTED: "import" swift-mode:keyword-face "class" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "Foo" swift-mode:type-face
import enum Foo.Foo // EXPECTED: "import" swift-mode:keyword-face "enum" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "Foo" swift-mode:type-face
import protocol Foo.Foo // EXPECTED: "import" swift-mode:keyword-face "protocol" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "Foo" swift-mode:type-face
import let Foo.foo // EXPECTED: "import" swift-mode:keyword-face "let" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "foo" swift-mode:property-access-face
import var Foo.foo // EXPECTED: "import" swift-mode:keyword-face "var" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "foo" swift-mode:property-access-face
import func Foo.foo // EXPECTED: "import" swift-mode:keyword-face "func" swift-mode:keyword-face "Foo" swift-mode:type-face "." swift-mode:operator-face "foo" swift-mode:function-name-face


// Config keywords
@available(macOS 256, *) // EXPECTED: "@available" swift-mode:attribute-face "(" swift-mode:bracket-face "macOS" swift-mode:build-config-keyword-face "256" swift-mode:number-face "," swift-mode:delimiter-face "\*" swift-mode:operator-face ")" swift-mode:bracket-face


// Precedence groups
infix operator +++: AdditionPrecedence // EXPECTED: "infix" swift-mode:keyword-face "operator" swift-mode:keyword-face "+++" swift-mode:operator-face ":" swift-mode:delimiter-face "AdditionPrecedence" swift-mode:builtin-precedence-group-face


// Constants
let x = [true, false, nil] // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "[" swift-mode:bracket-face "true" swift-mode:constant-keyword-face "," swift-mode:delimiter-face "false" swift-mode:constant-keyword-face "," swift-mode:delimiter-face "nil" swift-mode:constant-keyword-face "]" swift-mode:bracket-face


// Function/method calls
foo() // EXPECTED: "foo" swift-mode:function-call-face "()" swift-mode:bracket-face
foo<A>() // EXPECTED: "foo" swift-mode:function-call-face "<" swift-mode:operator-face "A" swift-mode:type-face ">" swift-mode:operator-face "()" swift-mode:bracket-face
foo { // EXPECTED: "foo" swift-mode:function-call-face "{" swift-mode:bracket-face
}
foo?() // EXPECTED: "foo" swift-mode:function-call-face "?" swift-mode:operator-face "()" swift-mode:bracket-face
foo? { // EXPECTED: "foo" swift-mode:function-call-face "?" swift-mode:operator-face "{" swift-mode:bracket-face
}


// Function definition
func foo<A>() { // EXPECTED: "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "<" swift-mode:operator-face "A" swift-mode:type-face ">" swift-mode:operator-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
}


// Negation operator
let x = !false // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "!" swift-mode:negation-char-face "false" swift-mode:constant-keyword-face
// Not negation operators. // EXPECTED: "//" swift-mode:comment-delimiter-face " Not negation operators." swift-mode:comment-face
let x = foo! + 1 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "!" swift-mode:operator-face "+" swift-mode:operator-face "1" swift-mode:number-face
let x = foo!(1) // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "!" swift-mode:operator-face "(" swift-mode:bracket-face "1" swift-mode:number-face ")" swift-mode:bracket-face
let x = foo != 1 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "!=" swift-mode:operator-face "1" swift-mode:number-face
let x = try! foo() as! Foo // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "try" swift-mode:keyword-face "!" swift-mode:operator-face "foo" swift-mode:function-call-face "()" swift-mode:bracket-face "as" swift-mode:keyword-face "!" swift-mode:operator-face "Foo" swift-mode:type-face
// Custom operator // EXPECTED: "//" swift-mode:comment-delimiter-face " Custom operator" swift-mode:comment-face
let x = foo +!+!+!+ 1 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "+!+!+!+" swift-mode:operator-face "1" swift-mode:number-face


// Preprocessor keywords
#if available(macOS 256, *) // EXPECTED: "#if" swift-mode:preprocessor-keyword-face "available" swift-mode:function-call-face "(" swift-mode:bracket-face "macOS" swift-mode:build-config-keyword-face "256" swift-mode:number-face "," swift-mode:delimiter-face "\*" swift-mode:operator-face ")" swift-mode:bracket-face
#endif // EXPECTED: "#endif" swift-mode:preprocessor-keyword-face


// Property accesses
let x = foo.foo // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "." swift-mode:operator-face "foo" swift-mode:property-access-face
    + foo.foo[0] // EXPECTED: "+" swift-mode:operator-face "." swift-mode:operator-face "foo" swift-mode:property-access-face "[" swift-mode:bracket-face "0" swift-mode:number-face "]" swift-mode:bracket-face
    + foo.foo?[0] // EXPECTED: "+" swift-mode:operator-face "." swift-mode:operator-face "foo" swift-mode:property-access-face "?" swift-mode:operator-face "[" swift-mode:bracket-face "0" swift-mode:number-face "]" swift-mode:bracket-face
    + foo.0 // known-bug: Should be property access.
    + foo.foo<A> // known-bug: Should be property access.
    + foo.foo(x:y:) // known-bug: Should be property access.

switch foo {
  case .foo: // EXPECTED: "case" swift-mode:keyword-face "." swift-mode:operator-face "foo" swift-mode:property-access-face ":" swift-mode:delimiter-face
}

let x = \.foo // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "." swift-mode:operator-face "foo" swift-mode:property-access-face


// Comment delimiters and comments

// aaa // EXPECTED: "//" swift-mode:comment-delimiter-face " aaa" swift-mode:comment-face

/* // EXPECTED: "/\*" swift-mode:comment-delimiter-face
 * aaa // EXPECTED: " " swift-mode:comment-face "\*" swift-mode:comment-delimiter-face " aaa" swift-mode:comment-face
 */ // EXPECTED: " " swift-mode:comment-face "\*/" swift-mode:comment-delimiter-face

/**/ // EXPECTED: "/\*\*/" swift-mode:comment-delimiter-face


// Documentation and documentation markups

/// documentation // EXPECTED: "///" swift-mode:comment-delimiter-face " documentation" swift-mode:doc-face
/// // EXPECTED: "///" swift-mode:comment-delimiter-face
/// - Parameters: // EXPECTED: "///" swift-mode:comment-delimiter-face " - " swift-mode:doc-face "Parameters" swift-mode:doc-markup-face ":" swift-mode:doc-face
///   - foo: The foo. // EXPECTED: "///" swift-mode:comment-delimiter-face "   - foo: The foo." swift-mode:doc-face
/// - Parameter bar: The bar. // EXPECTED: "///" swift-mode:comment-delimiter-face " - " swift-mode:doc-face "Parameter" swift-mode:doc-markup-face " bar: The bar." swift-mode:doc-face
/// - Returns: The return value. // EXPECTED: "///" swift-mode:comment-delimiter-face " - " swift-mode:doc-face "Returns" swift-mode:doc-markup-face ": The return value." swift-mode:doc-face
/// - Throws: An error // EXPECTED: "///" swift-mode:comment-delimiter-face " - " swift-mode:doc-face "Throws" swift-mode:doc-markup-face ": An error" swift-mode:doc-face

/** // EXPECTED: "/\*\*" swift-mode:comment-delimiter-face
 documentation // EXPECTED: " documentation" swift-mode:doc-face

 - Parameters: // EXPECTED: " - " swift-mode:doc-face "Parameters" swift-mode:doc-markup-face ":" swift-mode:doc-face
   - foo: The foo. // EXPECTED: "   - foo: The foo." swift-mode:doc-face
 - Parameter bar: The bar. // EXPECTED: " - " swift-mode:doc-face "Parameter" swift-mode:doc-markup-face " bar: The bar." swift-mode:doc-face
 - Returns: The return value. // EXPECTED: " - " swift-mode:doc-face "Returns" swift-mode:doc-markup-face ": The return value." swift-mode:doc-face
 - Throws: An error // EXPECTED: " - " swift-mode:doc-face "Throws" swift-mode:doc-markup-face ": An error" swift-mode:doc-face
 */ // EXPECTED: " " swift-mode:doc-face "\*/" swift-mode:comment-delimiter-face


// Types
class Foo: Foo { // EXPECTED: "class" swift-mode:keyword-face "Foo" swift-mode:type-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
}


// Strings
let foo = "abc" + """ // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "\"abc\"" swift-mode:string-face "+" swift-mode:operator-face "\"\"\"" swift-mode:string-face
  aaa // EXPECTED: "  aaa" swift-mode:string-face
  bbb // EXPECTED: "  bbb" swift-mode:string-face
  ccc // EXPECTED: "  ccc" swift-mode:string-face
  """ + ###""" // EXPECTED: "  \"\"\"" swift-mode:string-face "+" swift-mode:operator-face "###\"\"\"" swift-mode:string-face
  aaa // EXPECTED: "  aaa" swift-mode:string-face
  """ // EXPECTED: "  \"\"\"" swift-mode:string-face
  bbb // EXPECTED: "  bbb" swift-mode:string-face
  """ // EXPECTED: "  \"\"\"" swift-mode:string-face
  ccc // EXPECTED: "  ccc" swift-mode:string-face
  """### // EXPECTED: "  \"\"\"###" swift-mode:string-face


// Escaped identifiers and raw identifiers
`escaped_identifier`() // EXPECTED: "`escaped_identifier`" swift-mode:escaped-identifier-face "()" swift-mode:bracket-face
`raw identifier`() // EXPECTED: "`raw identifier`" swift-mode:escaped-identifier-face "()" swift-mode:bracket-face


// Delimiters
let x: [Int] = [1, 2, 3]; // EXPECTED: "let" swift-mode:keyword-face ":" swift-mode:delimiter-face "[" swift-mode:bracket-face "Int" swift-mode:builtin-type-face "]" swift-mode:bracket-face "=" swift-mode:operator-face "[" swift-mode:bracket-face "1" swift-mode:number-face "," swift-mode:delimiter-face "2" swift-mode:number-face "," swift-mode:delimiter-face "3" swift-mode:number-face "]" swift-mode:bracket-face ";" swift-mode:delimiter-face


// Escape sequences
let x = "aaa\"bbb\\ccc\u{FEFF}ddd" // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "\"aaa" swift-mode:string-face "\\\"" (swift-mode:escape-face swift-mode:string-face) "bbb" swift-mode:string-face "\\\\" (swift-mode:escape-face swift-mode:string-face) "ccc" swift-mode:string-face "\\u{FEFF}" (swift-mode:escape-face swift-mode:string-face) "ddd\"" swift-mode:string-face
let x = ###"aaa\"bbb\###"###ccc"### // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "###\"aaa\\\"bbb" swift-mode:string-face "\\###\"" (swift-mode:escape-face swift-mode:string-face) "###ccc\"###" swift-mode:string-face
let x = /aaa\/bbb/ // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "/aaa" swift-mode:regexp-face "\\/" (swift-mode:escape-face swift-mode:regexp-face) "bbb/" swift-mode:regexp-face
// In regexp, backslashes without pounds are still special.
let x = ###/aaa/bbb\/###ccc/### // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "###/aaa/bbb" swift-mode:regexp-face "\\/" (swift-mode:escape-face swift-mode:regexp-face) "###ccc/###" swift-mode:regexp-face
let x = ###/ // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "###/" swift-mode:regexp-face
  aaa // EXPECTED: "  aaa" swift-mode:regexp-face
  / // EXPECTED: "  /" swift-mode:regexp-face
  bbb // EXPECTED: "  bbb" swift-mode:regexp-face
  \/###ccc // EXPECTED: "  " swift-mode:regexp-face "\\/" (swift-mode:escape-face swift-mode:regexp-face) "###ccc" swift-mode:regexp-face
  /### // EXPECTED: "  /###" swift-mode:regexp-face


// Misc punctuations
let x = "aaa \(1 + (1 + 1)) bbb" // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "\"aaa " swift-mode:string-face "\\" (swift-mode:misc-punctuation-face swift-mode:string-face) "(" (swift-mode:misc-punctuation-face swift-mode:string-face) "1" swift-mode:number-face "+" swift-mode:operator-face "(" swift-mode:bracket-face "1" swift-mode:number-face "+" swift-mode:operator-face "1" swift-mode:number-face ")" swift-mode:bracket-face ")" (swift-mode:misc-punctuation-face swift-mode:string-face) " bbb\"" swift-mode:string-face
let x = ###"aaa \###(1 + (1 + 1)) \(1 + (1 + 1)) bbb"### // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "###\"aaa " swift-mode:string-face "\\" (swift-mode:misc-punctuation-face swift-mode:string-face) "###" (swift-mode:misc-punctuation-face swift-mode:string-face) "(" (swift-mode:misc-punctuation-face swift-mode:string-face) "1" swift-mode:number-face "+" swift-mode:operator-face "(" swift-mode:bracket-face "1" swift-mode:number-face "+" swift-mode:operator-face "1" swift-mode:number-face ")" swift-mode:bracket-face ")" (swift-mode:misc-punctuation-face swift-mode:string-face) " \\(1 + (1 + 1)) bbb\"###" swift-mode:string-face


// Numbers
let x100 = 100_000 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "100_000" swift-mode:number-face
let x0b1 = 0b100_000 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "0b100_000" swift-mode:number-face
let x0o1 = 0o100_000 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "0o100_000" swift-mode:number-face
let x0x1 = 0xFF_FF_FF // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "0xFF_FF_FF" swift-mode:number-face
let x1e1 = -10.5e-2 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "-10.5e-2" swift-mode:number-face
let x0x1p1 = 0xff.abp-2 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "0xff.abp-2" swift-mode:number-face


// Operators
let x = 1 + 1 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "1" swift-mode:number-face "+" swift-mode:operator-face "1" swift-mode:number-face
let x = 1 +++ 1 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "1" swift-mode:number-face "+++" swift-mode:operator-face "1" swift-mode:number-face
let x = 1 ..< 1 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "1" swift-mode:number-face "..<" swift-mode:operator-face "1" swift-mode:number-face
let x = true ? 1 : 1 // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "true" swift-mode:constant-keyword-face "?" swift-mode:operator-face "1" swift-mode:number-face ":" swift-mode:operator-face "1" swift-mode:number-face
let x = 1 ⊕ 1 // known-bug: Unicode operators are operators
foo<A>() // known-bug: <> should be brackets.


// Brackets
let x = {([])} // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "{([])}" swift-mode:bracket-face


// Regexps
let x = ###/abc/### // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "###/abc/###" swift-mode:regexp-face
let x = /abc/ // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "/abc/" swift-mode:regexp-face
let x = #//# // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "#//#" swift-mode:regexp-face
let x = ###/ // EXPECTED: "let" swift-mode:keyword-face "=" swift-mode:operator-face "###/" swift-mode:regexp-face
  aaa // EXPECTED: "  aaa" swift-mode:regexp-face
  /### // EXPECTED: "  /###" swift-mode:regexp-face
