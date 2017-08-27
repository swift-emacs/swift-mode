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
}
