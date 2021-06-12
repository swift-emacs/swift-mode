// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

// Simple types

let foo: A
  = abc

let foo:
  A = abc

let foo
  :A = abc

class Foo:
  A
    .A,
  B, C,
  D {
}

class Foo
  : A
      .A,
    B, C,
    D {
}


class Foo: A
             .A
         , B , C
         , D
{
}

class Foo
  : A.
      A
  , B , C
  , D
{
}


// Types with attribute

let foo: @A A
  = abc

let foo: @A
  A =
  abc

let foo:
  @A
  A =
  abc

let foo
  :@A
  A =
  abc

class Foo:
  @A
  A
    .A,
  B {
}

class Foo
  : @A
    A
      .A,
    B {
}

class Foo: @A
           A
             .A
         , B
{
}

class Foo
  : @A
    A
      .A
  , B
{
}

// Member types

let foo:
  /* */ A.
  /* */ B = abc

let foo:
  /* */ A
  /* */ .B = abc

class Foo:
  A.
    B,
  A.
    B,
  A
    .B {
}

class Foo
  : A.
      B,
    A.
      B,
    A
      .B {
}

class Foo: A.
             B
         , A.
             B
         , A
             .B
{
}

class Foo
  : A.
      B
  , A.
      B,
  , A
      .B {
}

// Array types

let foo: [
  A
]
  = abc

let foo:
  [
    A
  ] = abc

let foo
  :[
    A
  ] = abc

// Tuple types

let foo: (
  /* */ A,
  B
)
  = abc

let foo:
  (
    /* */ A,
    B
  ) = abc

let foo
  :(
    /* */ A,
    B
  ) = abc

// Dictionary types

let foo: [
  /* */ A:
    B
]
  = abc

let foo:
  [
    /* */ A:
      B
  ] = abc

let foo
  :[
    /* */ A:
      B
  ] = abc

// Function types

let foo: (
  A,
  B
)
  throws
  ->
  (
    A,
    B
  )
  throws
  ->
  [
    A
  ]
  = abc


let foo:
  (
    A,
    B
  )
  throws
  ->
  (
    A,
    B
  )
  throws
  ->
  [
    B
  ]
  = abc

let foo
  :(
    A,
    B
  )
  throws
  ->
  B
  = abc

let foo:
  (A, B)
  rethrows
  ->
  B
  = abc

let foo
  :(A, B)
  rethrows
  ->
  B
  = abc

let foo:
  (A, B)
  async
  throws
  ->
  (A)
  async
  rethrows
  ->
  (A)
  async
  ->
  B
  = abc


// Optional types

let foo: A?
  = abc

let foo:
  A? = abc

let foo: A!
  = abc

let foo:
  A! = abc

// Protocol composition types

let foo: protocol<A<[B]>,
                  C<(D, E)>>
  = a

let foo: protocol<
  A, // swift-mode:test:known-bug
  B
>
  = a
