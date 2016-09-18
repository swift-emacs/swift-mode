// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

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

    @ABC
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
      = g
      :
      (
        Int,
        Int
      )
      ->
      throws
      [
        X
      ]


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
          get {
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
}

// Function declarations

@A
private
  final
  func
  foo<A, B>(
    x:
      Int
    y:
      Int
      =
      1
    z
    w:
      Int
      ...
  )
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
           1, // swift-mode:test:known-bug
         B =
           2,
         C =
           3
    case D
           = 1, // swift-mode:test:known-bug
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

// Protocol declarations

protocol Foo {
    func foo(x, y) -> throws (A, B)
    init<A, B>(x: Int) throws
      where
        A: C
    subscript(x: Int) -> Int {
        get
        set
    }
    associatedtype AAA = BBB
    convenience
      init(x: Int, y, Int)
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
