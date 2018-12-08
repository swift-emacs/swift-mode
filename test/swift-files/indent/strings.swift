// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

func f() {
    let x = """
      aaa
      "
      aaa
      ""
      aaa
      \"""
      aaa
      ""
      aaa
      "
      aaa
      """ +
      "abc"

    let x = """
      aaa
      "
        aaa // swift-mode:test:keep-indent
        ""
        aaa
        \"""
        aaa
        ""
        aaa
        "
        aaa
      """ +
      "abc"

    let x = """
      aaa
      \(
        abc +
          def(a, b()) +
          """
          aaa
          \(
            bbb() +
              ccc() {
                  ddd()
              } +
              eee()
          )
          aaa
          """
      )
      aaa
      \(
        foo)
      aaa
      \(foo)
      aaa
      \(foo
      )
      aaa
      aaa\(
        foo() +
          bar()
      )aaa
      aaa
      """ +
      "abc"

    foo()

    let x = "aaa\(
      foo() +
        bar() +
        "abc"
    )aaa\(
      foo() +
        bar()
    )aaa"

    let x = ("aaa\(
               foo() +
                 bar() +
                 "abc"
             )aaa\(
               foo() +
                 bar()
             )aaa")

    let x = (
      "aaa\(
        foo() +
          bar() +
          "abc"
      )aaa\(
        foo() +
          bar()
      )aaa"
    )

    let x = """
      abc \(
        1 +
          (
            1 +
              1
          ) +
          1
      )
      def ghi
      \"""
      aaa
      """

    let x = #"""
      abc \(
      1 +
      (
      1 + 1
      ) +
      1
      )
      abc \#(
        1 +
          (
            1 + 1
          ) +
          1
      )
      def ghi
      \"""
      \#"""#
      """
      aaa
      """#

    let x = ##"""
      abc \(
      1 +
      (
      1 + 1
      ) +
      1
      )
      abc \#(
      1 +
      (
      1 + 1
      ) +
      1
      )
      def ghi
      \"""
      \#"""#
      """
      aaa
      """##

    let x = "abc\( 1 + (2 + 3) ) \" a "
    let x = #"abc\( 1 + (2 + 3) ) \#( 1 + (2 + 3) ) \" \#"# " a \"#
    let x = ##"abc\( 1 + (2 + 3) ) \#( 1 + (2 + 3) ) \" \#"# " a \"##
    let x = 1
}
