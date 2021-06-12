// /*[*/Foo bar baz./*]*/

// /*[*/Import declarations/*]*/

/*{*//* /*[*/aaa/*]*/ */ /* /*[*/bbb/*]*/ */@Foo import Foundation/*}*/
/*{*/@Foo
import Foundation/*}*/

/*{*/class Foo {
    /*{*/@Foo import Foundation/*}*/
    /*{*/@Foo
    import Foundation/*}*/
}/*}*/

// /*[*/Constant/variable declarations/*]*/

/*{*/let x = foo()/*}*/
/*{*/@Foo
let
  y
  =
  bar()/*}*/

/*{*/class Foo {
    /*{*/let x = foo()/*}*/
    /*{*/@Foo
    public
      let
      y
      =
      bar()/*}*/

    /*{*/var a: Int {
        /*[*/return 0/*]*/
    }/*}*/

    /*{*/var a: Int {
        /*{*/@Foo
        get {
            /*[*/return 0/*]*/
        }/*}*/

        /*{*/@Foo
        set {
            /*[*/foo()/*]*/
        }/*}*/
     }/*}*/

    /*{*/var a = 0 {
        /*{*/@Foo
        willSet {
            /*[*/foo()/*]*/
        }/*}*/

        /*{*/@Foo
        didSet {
            /*[*/foo()/*]*/
        }/*}*/
    }/*}*/

    /*{*/class func foo() {
        /*[*/let x = foo()/*]*/
        /*[*/let
          y
          =
          bar()/*]*/
        /*[*/while
          let
            x
            =
            xx,
          var
            y
            =
            yy,
          case
            (
              a,
              b
            )
            =
            ab {
            /*[*/foo()/*]*/
            /*[*/foo()/*]*/
        }/*]*/
    }/*}*/
}/*}*/

// /*[*/Type alias declarationss/*]*/

/*{*/@Foo typealias A = B/*}*/

/*{*/@Foo
typealias
  A
  =
  B/*}*/

/*{*/class Foo {
    /*{*/@Foo typealias A = B/*}*/

    /*{*/@Foo
    public
      typealias
      A
      =
      B/*}*/
}/*}*/

// /*[*/Function declarations/*]*/

/*{*/func foo() {
}/*}*/

/*{*/@Foo
func
foo() {
}/*}*/

/*{*/class Foo {
    /*{*/func foo() {
    }/*}*/

    /*{*/@Foo
    public
      func
      foo<A
            :
            X,
          B,
          C>(
        a a
          :
          (Int -> [Int])
          =
          {
              /*[*/x
              in/*]*/
              /*[*/[
                x
              ]/*]*/
          }
      )
      async
      throws
      ->
      @Foo
      [
        A
      ]
      where
        A
          :
          X
          ,
        B
          ==
          Int
    {
        /*[*/foo()/*]*/
    }/*}*/
}/*}*/

// /*[*/Enum declarations/*]*/

/*{*/enum Foo<A> where A: B {
    /*{*/case Foo(a: Int)/*}*/
    /*{*/case Bar(b: Int), Baz/*}*/
    /*{*/case
      A(
        b
          :
          Int)
            ,
      @Foo
      indirect
        B
        =
        0/*}*/

    /*{*/func foo() -> a {
        /*[*/switch this {
        /*[*/case .Foo:
            /*[*/return a/*]*/
        /*[*/case
          let .Bar(a):
            /*[*/return a/*]*/
        /*[*/case
          .Baz(var a):
            /*[*/return a/*]*/
        }/*]*/
    }/*}*/
}/*}*/


// /*[*/Struct declarations/*]*/

/*{*/struct Foo {
}/*}*/

// /*[*/Actor declarations/*]*/

/*{*/actor Foo {
}/*}*/

// /*[*/Class declarations/*]*/

/*{*/class Foo {
}/*}*/

/*{*/@Foo
public
  final
  class
  Foo<A
        :
        X
     ,
      B
     ,
      C>
  :
  X
       where
         A
           :
           X
           ,
         B
           ==
           Int
{
    /*{*/class Foo {
    }/*}*/
}/*}*/

// /*[*/Protocol declarations/*]*/

/*{*/protocol Foo {
    /*{*/var x: Int {
        /*{*/get/*}*/
        /*{*/set/*}*/
    }/*}*/
    /*{*/func foo()/*}*/

    /*{*/associatedtype
      A
      :
      B
      =
      C
      where
        A
          :
          D,
        A
          ==
          E/*}*/
}/*}*/

// /*[*/Extension declarations/*]*/
/*{*/extension Foo: AAA {
}/*}*/

// /*[*/Operator declarations/*]*/
/*{*/prefix
  operator
  +++/*}*/
/*{*/postfix
  operator
  +++/*}*/
/*{*/infix
  operator
  +++
  :
  AAA/*}*/

// /*[*/Precedence group declarations/*]*/
/*{*/precedencegroup Foo {
    /*[*/higherThan: AAA, BBB, CCC/*]*/
    /*[*/lowerThan: DDD, EEE, FFF/*]*/
    /*[*/assignment: false/*]*/
    /*[*/associativity: left/*]*/
}/*}*/

/*{*/class Foo {
    // /*[*/Initializer declarations/*]*/
    /*{*/init() {
        /*[*/`init`() {
        }/*]*/
    }/*}*/

    // /*[*/Deinitializer declarations/*]*/
    /*{*/deinit() {
    }/*}*/

    // /*[*/Subscript declarations/*]*/
    /*{*/subscript(x: Int) {
    }/*}*/
}/*}*/

// /*[*/Multiple declaratoins in single line/*]*/

/*{*/func foo(){};/*}*/ /*{*/func foo(){/*{*/func foo(){}/*}*/};/*}*//*{*/func foo(){} ;/*}*/ /*{*/func foo() {} /* */ ;/*}*/ /*{*//* */ func foo() {}/*}*/

// /*[*/Strings and comments/*]*/

/*{*/let x = """
  /*[*/class Foo {}
  \(
    { /*[*/() in/*]*/
        /*{*/class Foo {
        }/*}*/
        /*[*/return 0/*]*/
    }()
  )
  """/*}*/

// /*[*/class Foo {}/*]*/

/*
 /*[*/class Foo {
 }/*]*/
 */

// /*[*/Foo bar baz./*]*/
