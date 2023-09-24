// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

// Prefix expressions
// See also operators.swift

foo(
  +a,
  +b,
  &x
)

// Try and await operators

let foo = try
  a() + try
  b()

let foo = try!
  a() + try!
  b()

let foo = try?
  a() + try?
  b()

let foo = a+try
  a() +b+try
  b()

let foo = a+try!
  a() + b+try!
  b()

let foo = a+try?
  a() + b+try?
  b()


let foo = await
  a() + await
  b()

let foo = a+await
  a() +b+await
  b()

let foo = try await
  a() + try await
  b()

let foo = a+try await
  a() +b+try await
  b()

let foo = try
  await
  a() + try
  await
  b()

let foo = a+try
  await
  a() +b+try
  await
  b()


// Binary expressions
// See also operators.swift

let foo = 1 +
  /* */ 2 +
  3 + 4 +
  /* */ 5

let foo = 1 +
  2 +
    3 + 4 + // swift-mode:test:keep-indent
    5

// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 4)

let foo = 1 +
    2 +
    3 + 4 +
    5

// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)

// Ternary conditional operator

let foo = a()
  ? b()
  : c()

let foo = a() ?
  b() :
  c()

// Type-casting operators

let foo = a is
  A || b is
  B

let foo = a
  is A || b
  is B

let foo = a as
  A +++ b as
  B

let foo = a
  as A +++ b
  as B

let foo = a as?
  A +++ b as?
  B

let foo = a
  as? A +++ b
  as? B

let foo = a as!
  A +++ b as!
  B

let foo = a
  as! A +++ b
  as! B

// Literal expression

// Special literal

let foo =
  #file

let foo = #file
  +++ #function

if
  #file == a {
}

// Array literal

let x = [
  1,
  2,
  3, 4,
  5
]

let x = [
  1,
  2,
  3, 4,
  5,
]

let x =
  [
    1,
    2,
    3, 4,
    5
  ]

let x = [ 1,
          2,
          3, 4,
          5 ]

let x =
  [ 1
  , 2
  , 3, 4
  , 5
  ]

// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 3)

let x = [
   1,
   2,
   3, 4,
   5
]

let x = [
   1,
   2,
   3, 4,
   5,
]

let x =
  [
     1,
     2,
     3, 4,
     5
  ]

let x = [ 1,
          2,
          3, 4,
          5 ]

let x =
  [ 1
  , 2
  , 3, 4
  , 5 ]

// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)

// Dictionary literal

let x = [
  aaa:
    aaa
    + aaa,
  aaa:
    aaa
    + aaa,
  aaa: aaa, aaa:
              aaa
              + aaa,
  aaa
    : aaa
    + aaa,
  aaa: aaa, aaa
              : aaa
              + aaa,
  aaa
    : aaa
    + aaa
]

let x =
  [
    aaa:
      aaa
      + aaa,
    aaa:
      aaa
      + aaa,
    aaa: aaa, aaa:
                aaa
                + aaa,
    aaa
      : aaa
      + aaa,
    aaa: aaa, aaa
                : aaa
                + aaa,
    aaa
      : aaa
      + aaa
  ]

let x = [ aaa:
            aaa
            + aaa,
          aaa:
            aaa
            + aaa,
          aaa: aaa, aaa:
                      aaa
                      + aaa,
          aaa:
            aaa
            + aaa,
          aaa
            : aaa
            + aaa,
          aaa: aaa, aaa
                      : aaa
                      + aaa,
          aaa
            : aaa
            + aaa ]

let x = [
  :
]

let x =
  [
    :
  ]

let x = [ :
]

// Closure expressions

let x = {
    println("Hello, World!")
    println("Hello, World!")
}

let x = { x in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = { x
          in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = {
    x in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = {
    x
    in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = { (
            x: Int,
            y: Int
          )
            async
            throws
            ->
            Foo
          in
    println("Hello, World! " + x + y)
    println("Hello, World! " + x + y)

    return foo
}

let x = { [
            weak
              self,
            unowned(unsafe)
              foo
          ]
          (
            x: Int,
            y: Int
          )
            async
            throws
            ->
            Foo
          in
    println("Hello, World! " + x + y)
    println("Hello, World! " + x + y)

    return foo
}

let x = {
    [
      weak self,
      weak foo
    ]
    (
      x: Int,
      y: Int
    )
      async
      throws
      ->
      Foo
    in
    println("Hello, World! " + x + y)
    println("Hello, World! " + x + y)

    return foo
}


let x = { a,
          b,
          c, d,
          e
          in
    println("Hello, World! " + x + y)
}

let x = {
    a,
    b,
    c, d,
    e
    in
    println("Hello, World! " + x + y)
}

let x = { a
        , b
        , c, d
        , e
          in
    println("Hello, World! " + x + y)
}

// Implicit member expressions

x =
  .aaa

// Parenthesized expressions

let x = (
  a,
  b, c,
  d,
  e,
  ++,
  **, --,
  aaa
)

let x = (
  aaa:
    a,
  aaa: b, aaa:
            c,
  aaa:
    d,
  aaa
    :e,
  aaa:
    ++,
  aaa: **, aaa:
             --,
  aaa:
    aaa
)

let x =
  (
    1,
    2,
    3, 4,
    5
  )

let x = ( 1,
          2,
          3, 4,
          5 )

let x =
  ( 1
  , 2
  , 3, 4
  , 5
  )

// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 3)


let x = (
   a,
   b, c,
   d,
   e,
   ++,
   **, --,
   aaa
)

let x = (
   aaa:
     a,
   aaa: b, aaa:
             c,
   aaa:
     d,
   aaa
     :e,
   aaa:
     ++,
   aaa: **, aaa:
              --,
   aaa:
     aaa
)

let x =
  (
     1,
     2,
     3, 4,
     5
  )

let x = ( 1,
          2,
          3, 4,
          5 )

let x =
  ( 1
  , 2
  , 3, 4
  , 5
  )

// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)

// Selector expressions

let x =
  #selector(
    Foo.bar(
      _:,
      aaa:,
      bbb:
    )
  )

let x =
  #selector(
    getter:
      Foo.ppp
  )

// Funcation call expressions and explicit member expressions

let x =
  foo(a,
      b,
      c)

let x =
  foo(
    a,
    b,
    c
  )

let x = foo
  .bar(a:
         aaa,
       b:
         bbb(c:
               c,
             c:
               c) { x in
             foo
             bar
         },
       c:
         aaaa[
           aaa
         ]
  ) { aaa in
      aaa
      aaa
      aaa
  }

let x = foo
  .bar() { a in
      aaa
  }

let x = foo
  .bar() {
      a in
      aaa
  }

let x = foo
  .bar() {
      a
      in
      aaa
  }

foo
(bar) // this is not a function call

// Subscript expression

foo[
  1,
  2, 3,
  4
]

foo
[1] // this is not a subscript expression


// Multiple Trailing Closures
// https://github.com/apple/swift-evolution/blob/master/proposals/0279-multiple-trailing-closures.md

let x = foo
  .bar {
      aaa()
  } baz: {
      aaa()
  }

let x = foo.bar {
    aaa()
} baz: {
    aaa()
}

let x =
  foo
  .bar {
      aaa()
  } baz: {
      aaa()
  }


// If expression and Switch expression
// https://github.com/apple/swift-evolution/blob/main/proposals/0380-if-switch-expressions.md

let a =
  if x { 1 }
  else if y { 2 }
  else { 3 }

let a = if x { 1 }
  else if y { 2 }
  else { 3 }

let a = if x
    { 1 } // swift-mode:test:known-bug
  else if y
    { 2 }
  else
    { 3 }

let a = if x { 1 } else if y { 2 } // swift-mode:test:known-bug
  else { 3 }

let a = if x { 1 } else if y { 2 } else
  { 3 }

let a = if x { 1 } else
  if y { 2 } else
    { 3 }

let a = if x { 1 }
  else
    if y { 2 }
    else { 3 }

let a =
  if x { 1 }
  else
    if y { 2 }
    else { 3 }

let a =
  if x { 1 } else
    if y { 2 } else
      { 3 }

let a = if x {
    1
} else if y {
    2
} else {
    3
}

let a = if
  x {
    1
} else if y {
    2
} else {
    3
}

let a = if x
             .foo()
             .bar() { foo() }
  else { bar() }

let a = if x
             .foo()
             .bar() {
    foo()
} else {
    bar()
}

let a =
  if
    x
      .foo()
      .bar() { foo() }
  else { bar() }

let a =
  if
    x
      .foo()
      .bar() {
      foo()
  } else {
      bar()
  }

let a = if
  let
    x
    =
    xx,
  var
    y
    =
    yy,
  x
    ==
    y,
  case
    (
      a,
      b
    )
    =
    ab {
    foo()
} else {
    bar()
}

func foo() -> Int {
    return if x { bar() }
      else { baz() }
}

func foo() -> Int {
    return
      if x { bar() }
      else { baz() }
}

func foo() -> Int {
    return
      if x {
          bar()
      }
      else {
          baz()
      }
}

func foo() -> Int {
    return if x {
        bar()
    }
      else {
        baz()
    }
}

let x = switch foo.bar {
case foo:
    foo()
      .bar()
default:
    foo()
}


let x = switch foo
  .bar {
case foo:
    foo()
default:
    foo()
}

let x = switch
  foo
  .bar {
case foo:
    foo()
    foo()
default:
    foo()
    foo()
}

func foo() {
    return switch x {
    case 1:
        1
    default:
        2
    }
}

func foo() {
    return
      switch x {
      case 1:
          1
      default:
          2
      }
}

// consume operator
// https://github.com/apple/swift-evolution/blob/main/proposals/0366-move-function.md

func foo() {
    // consume operator cannot be followed by a line break.
    let y =
      consume
    x

    return y
}


// copy operator
// https://github.com/apple/swift-evolution/blob/main/proposals/0377-parameter-ownership-modifiers.md

func foo() {
    // copy operator cannot be followed by a line break.
    copy
    x
}


// discard self
// https://github.com/apple/swift-evolution/blob/main/proposals/0390-noncopyable-structs-and-enums.md

func foo() {
    // discard operator cannot be followed by a line break.
    discard
    self
}
