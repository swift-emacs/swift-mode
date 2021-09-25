// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)


// Backquoted identifier must behave like normal identifier

enum `switch` {
    case 1
}

do {
} catch `case`
          where a


let foo = `var`
  .then {
  }

let x = `where` +
  a


// Keywords after dot must behave like normal identifier
// https://github.com/apple/swift-evolution/blob/master/proposals/0071-member-keywords.md

let foo = foo.var
  .then {
  }

let x = foo.where +
  a

// keywords as parameter names

func foo() {
    // Keywords used in declarations
    foo(
      associatedtype: 1
    )
    foo(
      class: 1
    )
    foo(
      deinit: 1
    )
    foo(
      enum: 1
    )
    foo(
      extension: 1
    )
    foo(
      fileprivate: 1
    )
    foo(
      func: 1
    )
    foo(
      import: 1
    )
    foo(
      init: 1
    )
    foo(
      inout: 1
    )
    foo(
      internal: 1
    )
    foo(
      let: 1
    )
    foo(
      open: 1
    )
    foo(
      operator: 1
    )
    foo(
      private: 1
    )
    foo(
      protocol: 1
    )
    foo(
      public: 1
    )
    foo(
      static: 1
    )
    foo(
      struct: 1
    )
    foo(
      actor: 1
    )
    foo(
      subscript: 1
    )
    foo(
      typealias: 1
    )
    foo(
      var: 1
    )
    foo(
      some: 1
    )
    foo(
      nonisolated: 1
    )
    foo(
      isolated: 1
    )

    // Keywords used in statements
    foo(
      break: 1
    )
    foo(
      case: 1
    )
    foo(
      continue: 1
    )
    foo(
      default: 1
    )
    foo(
      defer: 1
    )
    foo(
      do: 1
    )
    foo(
      else: 1
    )
    foo(
      fallthrough: 1
    )
    foo(
      for: 1
    )
    foo(
      guard: 1
    )
    foo(
      if: 1
    )
    foo(
      in: 1
    )
    foo(
      repeat: 1
    )
    foo(
      return: 1
    )
    foo(
      switch: 1
    )
    foo(
      where: 1
    )
    foo(
      while: 1
    )

    // Keywords used in expressions and types (without true, false, and keywords begin with a number sign)
    foo(
      as: 1
    )
    foo(
      catch: 1
    )
    foo(
      dynamicType: 1
    )
    foo(
      is: 1
    )
    foo(
      rethrows: 1
    )
    foo(
      super: 1
    )
    foo(
      self: 1
    )
    foo(
      Self: 1
    )
    foo(
      throws: 1
    )
    foo(
      throw: 1
    )
    foo(
      async: 1
    )
    foo(
      try: 1
    )
    foo(
      await: 1
    )

    // Keywords reserved in particular contexts
    foo(
      Protocol: 1
    )
    foo(
      Type: 1
    )
    foo(
      and: 1
    )
    foo(
      assignment: 1
    )
    foo(
      associativity: 1
    )
    foo(
      convenience: 1
    )
    foo(
      didSet: 1
    )
    foo(
      dynamic: 1
    )
    foo(
      final: 1
    )
    foo(
      get: 1
    )
    foo(
      higherThan: 1
    )
    foo(
      indirect: 1
    )
    foo(
      infix: 1
    )
    foo(
      lazy: 1
    )
    foo(
      left: 1
    )
    foo(
      lowerThan: 1
    )
    foo(
      mutating: 1
    )
    foo(
      none: 1
    )
    foo(
      nonmutating: 1
    )
    foo(
      optional: 1
    )
    foo(
      override: 1
    )
    foo(
      postfix: 1
    )
    foo(
      precedence: 1
    )
    foo(
      precedencegroup: 1
    )
    foo(
      prefix: 1
    )
    foo(
      required: 1
    )
    foo(
      right: 1
    )
    foo(
      set: 1
    )
    foo(
      unowned: 1
    )
    foo(
      weak: 1
    )
    foo(
      willSet: 1
    )
}

// Unicode identifiers

let こんにちは = 你好 +
  안녕하세요 +
  😊
