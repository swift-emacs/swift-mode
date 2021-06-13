[![License GPL 3][badge-license]][copying]
[![Run Tests][badge-run-test]][action-run-test]
[![MELPA](https://melpa.org/packages/swift-mode-badge.svg)](https://melpa.org/#/swift-mode)
[![MELPA](https://stable.melpa.org/packages/swift-mode-badge.svg)](https://melpa.org/#/swift-mode)

# swift-mode

Major-mode for Apple's [Swift programming language](https://developer.apple.com/swift/).

## Installation

Install `swift-mode` package from MELPA.

To install without MELPA, download [latest release](https://github.com/swift-emacs/swift-mode/releases) and execute `M-x package-install-file` for the .tar archive.

## Features

- Font Lock
- Indentation

  ```swift
  switch foo {
  case let .P1(x)
         where
           x > 0,
       let .P2(x)
         where
           x > 0:
      bar()
        .then { x in
            return baz(x)
        }
        .then {(
                 x,
                 y
               ) in
            return moo(x + y)
        }
  }

  // Hanging brace
  let x = [
    1,
    2,
    3
  ]

  // Brace on its own line
  let y =
    [
      1,
      2,
      3
    ]

  // Utrecht style
  let z =
    [ 1
    , 2
    , 3
    ]
  ```
- `forward-sexp`
- `beginning-of-defun`, `end-of-defun`, `mark-defun`, and `narrow-to-defun`.
- `beginning-of-sentence`, `end-of-sentence`, `kill-sentence`, `backward-kill-sentence`, `mark-sentence`, and `narrow-to-sentence`.
  A sentence is a statement outside comments or strings, or an ordinal sentence inside comments or strings.
- `indent-new-comment-line`
- [Imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html)
- Running Swift REPL in a buffer (`M-x run-swift`)
- Build Swift module (`M-x swift-mode:build-swift-module`)
- Build iOS app (`M-x swift-mode:build-ios-app`)
- Running debugger on Swift module (`M-x swift-mode:debug-swift-module`)
- Running debugger on iOS app in simulator or device (`M-x swift-mode:debug-ios-app`)
  ([`ios-deploy`](https://github.com/ios-control/ios-deploy) is required to debug on device).

This package does not provide flycheck. See [flycheck-swift](https://github.com/swift-emacs/flycheck-swift).

## Limitations

Some syntax constructs removed from Swift 3.0 are not supported:

- C-style for-loop: `for var i = 1; i < 10; i++ { }`
- Multiple assignments in single `if let`:
  ```swift
  if let x = x,
         y = y {
  }
  ```

  Use multiple `let` instead:
  ```swift
  if let x = x,
     let y = y {
  }
  ```

Indentation may not accurate. For example, `foo(Bar < A, B > (c))` can be indented like either
```swift
foo(Bar < A,
    B > (c)) // Passing two Boolean arguments to foo
```
or
```swift
foo(Bar < A,
          B > (c)) // Passing a new Bar with two type arguments and a value
```
The Swift compiler disambiguates this case using tokens after `>`, but those tokens may not available at editing time. We use some heuristic for this.

Another example is difficulty of handling of colons. We have to pair all `?` and `:` of conditional operators to decide indentation of the below snippet. This is a future work.

```swift
switch foo {
  case let P(x) where x is Foo? ? a ? b : c ?? d : e ? f : g :
    h ? i?.j() : k()
}

switch foo {
  case let P(x) where (x is Foo?) ? (a ? b : c ?? d) : (e ? f : g) :
    h ? i?.j() : k()
}
```

Yet another difficult case is consistency of blocks. We want to indent method chains like this:
```swift
var x = foo
  .then { x in
      aaa
  }
  .then { x in
      aaa
  }
```

while we also want to indent the body of `if` like this:

```swift
if anotherVeryLongVariableName
     .veryLongPropertyName {
    aaa
}
```

That is, we have to indent the closing brace with offset if it is a part of expressions while it should be aligned with the beginning of the statement/declaration if it is a part of a statement/declaration.

Then, how should we indent the following code when the cursor is before `@`?

```swift
var x = foo
  .bar {
    @
```

This could be
```swift
var x = foo
  .bar {
    @abc willSet {
        aaa
    }
}
// property declaration
```
or
```swift
var x = foo
  .bar {
      @abc var x = 1
      x
  }
// property initialization
```

Both are syntactically correct code. We cannot handle this case properly. This is also a future work.

## Hacking

To build the package locally, run `make package`.

To install the built package, run `make install`.

To run tests, run `make test`.

For other commands, run `make help`.

## Related projects

- [Official swift-mode.el by Apple](https://github.com/apple/swift/blob/master/utils/swift-mode.el): Seems still in very early stage for now. We cannot contribute to it due to the license incompatibility.
- [sourcekit-lsp](https://github.com/apple/sourcekit-lsp): Language Server Protocol implementation for Swift and C-based languages.
- [lsp-sourcekit](https://github.com/emacs-lsp/lsp-sourcekit): Emacs client for lsp-sourcekit.
- [swift-helpful](https://github.com/danielmartin/swift-helpful): Shows documentation about Swift keywords, attributes, and API.
- [company-sourcekit](https://github.com/nathankot/company-sourcekit): Completion for Swift projects via SourceKit with the help of SourceKitten.
- [flycheck-swift](https://github.com/swift-emacs/flycheck-swift): Flycheck extensions for Swift.
- [swift-playground-mode](https://gitlab.com/michael.sanders/swift-playground-mode): Emacs support for Swift playgrounds.
- [swift-format](https://github.com/apple/swift-format): Formatter for Swift by Apple (`swift format` command).
- [SwiftRewriter](https://github.com/inamiy/SwiftRewriter): Formatter for Swift using SwiftSyntax.
- [SwiftFormat](https://github.com/nicklockwood/SwiftFormat): Formatter for Swift.

## Contributing

Yes, please do! See [CONTRIBUTING](./CONTRIBUTING.md) for guidelines.

## Acknowledgements

The REPL code is based on [js-comint](http://js-comint-el.sourceforge.net/).

Thanks to the following original developer and users for their contributions:

- [@chrisbarrett](https://github.com/chrisbarrett) (Chris Barrett)
- [@ap4y](https://github.com/ap4y) (Arthur Evstifeev)
- [@bbatsov](https://github.com/bbatsov) (Bozhidar Batsov)
- [@ckruse](https://github.com/ckruse) (Christian Kruse)
- [@syohex](https://github.com/syohex) (Syohei Yoshida)
- [@uk-ar](https://github.com/uk-ar) (Yuuki Arisawa)
- [@msanders](https://github.com/msanders) (Michael Sanders)

You can find a [full list of those people here](https://github.com/swift-emacs/swift-mode/graphs/contributors).

Thanks to [@purcell](https://github.com/purcell) (Steve Purcell) for advices on the code and arrangement for merging `swift3-mode` and `swift-mode`.

## License

GPLv3. See [COPYING][] for details. Copyright (C) 2014-2021 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[badge-run-test]: https://github.com/swift-emacs/swift-mode/workflows/Run%20Tests/badge.svg
[action-run-test]: https://github.com/swift-emacs/swift-mode/actions?query=workflow%3A%22Run+Tests%22
[COPYING]: ./COPYING
