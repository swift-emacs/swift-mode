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


    // Regexes

    // Simple case.
    let x = /a/

    // Slashes can be escaped.
    let x = /\/ +/

    // Slashes must be escaped in character classes.
    let x = /[/ + "]/ + // "
    let x = /[\/ + "]/ + // "
      a()

    // Regexes can contain quotes.
    let x = /"/
    let x = /"""/

    // Regex with extended delimiters can contain slashes.
    let x = #// /* /#

    // Backslashes are still special in regexes with extended delimiters.
    let x = #/\/# /* /#
    a()

    // Multiline regex.
    let x = #/
      let x = #/
      /#

    // Closing extended delimiter can be escaped.
    let x = #/
      \/#
      let x = #/
      /#

    // Extended delimiters with more than one pound.
    let x = ##/
      /#
      let x = #/
      /##

    // Comments are ignored in exended regexes.
    let x = #/
      let x = "a" # /#
      /#

    // Multiline comment cannot contain regexes with */.
    /*
     let regex = /[0-9]*/
    let x = "*/ // "


    // Regexes without extended delimiters cannot be preceded by infix
    // operators without whitespaces.
    // `a`, infix operator `+/`, `b`, and infix operator `%/`
    let x = a+/b %/
      c()

    // Regexes without extended delimiters can be preceded by infix operators
    // with whitespaces.
    // `a`, infix operator `+`, and regex /b %/
    let x = a + /b %/
    c()

    // Comments are whitespaces.
    let x = a/**/+/**//b %/
    c()

    // Regexes with extended delimiters can be preceded by infix operators
    // without whitespaces.
    // `a`, infix operator `+`, and regex #/b /#
    let x = a+#/b /#
    c()

    // Regexes without extended delimiters cannot start with spaces.
    let regex = Regex {
        digit
        // infix operator `/`, and `a` with postfix operator `/'
          / a/
        digit
    }
    // Regexes without extended delimiters cannot end with spaces.
    let regex = Regex {
        digit
        // prefix operator `/`, `a`, and infix operator `/'
        /a /
          digit
    }
    let regex = Regex {
        digit
        // regex /a/
        /a/
        digit
    }

    // Initial space must be escaped.
    let regex = Regex {
        digit
        /\ a/
        digit
    }

    // Final space must be escaped.
    let regex = Regex {
        digit
        /a\ /
        digit
    }

    // Regexes with extended delimiters can start with spaces.
    let regex = Regex {
        digit
        #/ a /#
        digit
    }

    foo {
        // This must be infix operator /^/.
        let a = b() /^/
          c() // swift-mode:test:known-bug
    }

    foo {
        // Regex /^/, infix operator /^/, and b().
        let a = /^/ /^/
          b() // swift-mode:test:known-bug
    }

    foo {
        // Regex /^/, infix operator /^/, regex /^/, and b()
        let a = /^/ /^/ /^/
        b()
    }

    // Regex without extended delimiters cannot be multiline.
    // Also, it cannot end with // or /*
    let a = /0 + // /
      b()
    let a = /0 + /* /
                    b()
                  */
      c()

    // Regexes can be preceded with prefix operators wihtout spaces.
    // prefix operator `+` and regex /a %/.
    let x = +/a %/
    b()

    // Regexes without extended delimiters cannot contain unmatching close
    // parentheses.
    array.reduce(1,/) { otherArray.reduce(1,/)
                        array.reduce(1,/) }; otherArray.reduce(1,/)

    // Regexes without extended delimiters can contain matching close
    // parentheses.
    array.reduce(1,/(a) { otherArray.reduce(1,/)
    array.reduce(1,/(a) }; otherArray.reduce(1,/)

    // Regexes without extended delimiters can contain escaped close
    // parentheses.
    array.reduce(1,/\) { otherArray.reduce(1,/)
    array.reduce(1,/\) }; otherArray.reduce(1,/)

    // Character classes can contain closing parentheses.
    array.reduce(1,/[)] { otherArray.reduce(1,/)
    array.reduce(1,/[)] }; otherArray.reduce(1,/)

    // Regexes with extended delimiters can contain unmatching close
    // parentheses.
    array.reduce(1,#/) { otherArray.reduce(1,/#)
    array.reduce(1,#/) }; otherArray.reduce(1,/#)


    // Regexes can contain unmatching close square brackets.
    let d = a[/] %/
    ]
    let d = a[(/)] %/
      b()

    // Comments have higher precedence.
    let x = a() /**/+++/
      b()
    let x = a() //+++/
    b()
}
