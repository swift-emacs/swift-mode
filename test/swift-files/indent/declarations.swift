// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

// Import declaration

import
  Foo

import
  class
  Foo
  .Bar

import class
  Foo

import
  let
  Foo

import
  var
  Foo.
  bar

import
  func
  Foo.+

import
  func
  Foo
  .+

let x = 1  // swift-mode:test:known-bug

// Constant declarations

let
  foo
  .bar
  =
  bar
  .baz

class Foo {
    @ABC
    open
      weak
      let
      (
        x,
        y
      )
      :
      (
        Int,
        Int
      )
      =
      xx

    @ABC(aaa)
    final
      unowned(safe)
      fileprivate
      let
      Foo
      .Bar(x)
      :
      Foo
      .Bar
      =
      xx

    let f
      :
      (
        Int,
        Int
      )
      async
      throws
      ->
      [
        X
      ]
      = g


    let x = 1,
        y = 1,
        z = 1

    let
      x = 1,
      y = 1,
      z = 1

    let x = 1
      , y = 1
      , z = 1

    // Declaring multiple variables with single `let` statement doesn't seem to
    // be popular. Rather, we choose saving columns for the first variable.
    private final let x = foo
      .foo // This is intended.
      .foo,
                      y = foo
                        .then { x // This is intended.
                                in
                            foo

                            return foo
                        }
                        .then { x
                                in
                            foo

                            return foo
                        },
                      z = foo
                        .foo
                        .foo
}

// Variable declarations

class Foo {
    internal var x = foo
      .foo
      .foo,
                 y = foo
                   .foo
                   .foo,
                 z = foo
                   .foo
                   .foo

    internal var x
      : (Int, Int) {
        foo()

        return foo()
    }

    internal var x
      : (Int, Int) {
        @A
        mutating
          get
          async
          throws {
            foo()

            return foo()
        }

        @A
        mutating
          set
          (it) {
            foo()
            foo(it)
        }
    }

    internal var x
      : (Int, Int) {
        @A
        mutating
          get
          async
          throws

        @A
        mutating
          set
    }

    internal var x
      :
      (Int, Int)
      =
      foo
      .bar {
          return thisIsFunctionBlock
      } {
          // This is bad, but cannot decide indentation without looking forward
          // tokens.
          @A
          willSet(a) {
              foo()
              foo()
          }

          @A
          didSet(a) {
              foo()
              foo()
          }
      } // This is bad

    internal var x
      :
      (Int, Int) {
        @A
        willSet(a) {
            foo()
            foo()
        }

        @A
        didSet(a) {
            foo()
            foo()
        }
    }

    var x {
        get {
            1
        }

        set {
            foo()
        }
    }

    var x {
        get
        {
            1
        }

        set
        {
            foo()
        }
    }

    var x {
        get
          async
          throws {
            1
        }
    }

    var x {
        get
          async
          throws
        {
            1
        }
    }

    internal var x: Int {
        @A
        mutating
          get
          async
          throws

        @A
        mutating
          set
    }
}

// Type alias declaration

class Foo {
    typealias A<B> = C
      .D

    @A
    private typealias A<B>
      =
      C
      .D

    typealias Foo<
      A: A,
      A: A, A: A
    > =
      A

    typealias Foo<
      A: A,
      A: A, A: A> =
      A

    typealias Foo <A: A,
                   A: A, A: A> =
      A

    typealias Foo <A: A,
                   A: A, A: A
    > =
      A

    typealias Foo
      <
        A: A,
        A: A, A: A
      > =
      A

    typealias Foo
      <
        A: A,
        A: A, A: A> =
      A

    typealias Foo
      <A: A,
       A: A, A: A> =
      A

    typealias Foo
      <A: A,
       A: A, A: A
      > =
      A
}

// Function declarations

@A
private
  final
  func
  foo<A,
      B>
  (
    x:
      @A
      isolated
      inout
      Int,
    y:
      Int
      =
      1,
    z,
    w:
      Int
      ...
  )
  async
  throws
  ->
  [A]
  where
    A:
      C,
    B =
      C<D> {
    foo()
    foo()
}

func
  foo()
  ->
  @A
  B {
    foo()
    foo()
}

func
  foo()
  ->
  @A(aaa)
  B {
    foo()
    foo()
}

func foo<
  A: A,
  A: A, A: A
> (
  a: A,
  a: A
){
}

func foo<
  A: A,
  A: A, A: A> (
  a: A,
  a: A) {
}

func foo<A: A,
         A: A, A: A> (a: A,
                      a: A) {
}

func foo<A: A,
         A: A, A: A
> (
  a: A,
  a: A
) {
}

func foo
  <
    A: A,
    A: A, A: A
  >
  (
    a: A,
    a: A
  ){
}

func foo
  <
    A: A,
    A: A, A: A>
  (
    a: A,
    a: A) {
}

func foo
  <A: A,
   A: A, A: A>
  (a: A,
   a: A) {
}

func foo
  <A: A,
   A: A, A: A
  >
  (a: A,
   a: A
  ) {
}

// Enumeration declarations

fileprivate
  indirect
  enum
  Foo<A, B>
  : X,
    Y,
    Z
  where
    A:
      C,
    B =
      D<E> {
    @A
    case A
    case B
    case C,
         D,
         E
    indirect
      case
      F(
        x:
          X,
        y:
          Y
      ),
      G,
      H

    func foo() {
    }

    case I
    case J
}

fileprivate
  enum
  Foo<A, B>
  :
  Int
  where
    A:
      C,
    B =
      D<E> {
    case A =
           1,
         B =
           2,
         C =
           3
    case D
           = 1,
         E
           = 2,
         F
           = 3

    func foo() {
    }
}


enum Foo
  : X,
    Y,
    Z {
}

enum Foo
  : X
  , Y
  , Z
{
}

// Struct declarations
// See also types.swift

@A
fileprivate
  struct
  Foo<A, B>
  : Bar<A, B>,
    Baz<A, B>,
    AAA<A, B>
  where
    A:
      C,
    B =
      D<E> {
    func foo()
    func foo()
}


@A
@B
struct A {
    func foo() {
    }
    func foo() {
    }
}

@A(a)
@B(b)
struct A {
    func foo() {
    }
    func foo() {
    }
}

struct Foo<
  A: A,
  A: A, A: A
> {
}

struct Foo<
  A: A,
  A: A, A: A> {
}

struct Foo<A: A,
           A: A, A: A> {
}

struct Foo<A: A,
           A: A, A: A
> {
}

struct Foo
  <
    A: A,
    A: A, A: A
  > {
}

struct Foo
  <
    A: A,
    A: A, A: A> {
}

struct Foo
  <A: A,
   A: A, A: A> {
}

struct Foo
  <A: A,
   A: A, A: A
  > {
}

// Actor declarations

@A
fileprivate
  actor
  Foo<A, B>
  : Bar<A, B>,
    Baz<A, B>,
    AAA<A, B>
  where
    A:
      C,
    B =
      D<E> {
    func foo()
    nonisolated
      func foo()
}


// Protocol declarations

protocol Foo {
    func foo(x: Int, y: Int) throws -> (A, B)
    func bar(x: Int) throws
    func baz(x: () throws -> Int) rethrows
    init<A, B>(x: Int) throws
      where
        A: C
    subscript(x: Int) -> Int {
        get
          async
          throws
        set
    }
    associatedtype AAA = BBB
    convenience
      init(x: Int, y, Int)
    var foo: Int {
        get
          async
          throws
        set
    }
}

// Operator declarations

infix
  operator
  +++
  :
  precedenceGroupName

prefix
  operator
  +++

postfix
  operator
  +++

precedencegroup
  precedenceGroupName {
    higherThan:
      lowerGroupName
    lowerThan:
      higherGroupName
    assignment:
      false
    associativity:
      left
}

// Declaration modifiers

class Foo {
    open
      class
      mutating
      nonmutating
      func
      foo() {
    }

    public
      (
        set
      )
      class
      dynamic
      final
      lazy
      optional
      required
      static
      unowned
      unowned
      (
        safe
      )
      unowned
      (
        unsafe
      )
      weak
      var
      x = 1

    internal
      class
      let
      x = 1

    fileprivate
      class
      init() {
    }

    private
      class
      deinit {
    }

    class
      subscript(foo: Int) -> Int {
        return foo
    }
}

// async let

func foo(f: @escaping (Int, Int) async -> Int, t: (Int, Int)) async {
    let a = f as (Int, Int)
      async -> Int
    let b = t as (Int, Int)
    async
    let c = 1
}
