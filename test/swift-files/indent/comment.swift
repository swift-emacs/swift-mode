// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

// aaa
// bbb
// ccc
/*
 aa
 aa
 aa
 - aaa
 - bbb
 - ccc
 */

/* */ class Foo {
    // aaa
    // bbb
      // ccc // swift-mode:test:keep-indent
      // ddd
      /* // swift-mode:test:known-bug
         * aa
           * aa // swift-mode:test:keep-indent
           * aa
       */
}

@Annotation(aaa)
private
  /* */ final /*
               */ class /*
                         */ Foo /*
                                 */ {
    aaa()
    bbb()
}
