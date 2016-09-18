[![License GPL 3][badge-license]][copying]
[![Build Status][badge-travis]][travis]
[![MELPA](https://melpa.org/packages/swift-mode-badge.svg)](https://melpa.org/#/swift-mode)
[![MELPA](https://stable.melpa.org/packages/swift-mode-badge.svg)](https://melpa.org/#/swift-mode)

# swift-mode

Major-mode for Apple's [Swift programming language](https://developer.apple.com/swift/).

## Installation

Install `swift-mode` package from MELPA.

To install without MELPA, download [latest release](https://github.com/swift-emacs/swift-mode/releases) and execute `M-x package-install-file`.

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
- `beginning-of-defun` and `end-of-defun`
- `indent-new-comment-line`
- [Imenu](https://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html)
- Running Swift REPL in a buffer (`M-x run-swift`)

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

Indentation may not accurate. For example, `foo(Bar < A, B > (c))` is ambiguous. It is indented like either
```swift
foo(Bar < A,
    B > (c)) // Passing two Boolean arguments to foo
```
or
```swift
foo(Bar < A,
          B > (c)) // constructing Bar with two type arguments and a value
```
The Swift compiler disambiguates this case using tokens after `>`, but those tokens may not available in editing time. We use some heuristic for this.

Another example is difficulty of handling of colons. We have to pair all `?` and `:` of conditional operators to decide indentation. This is a future work.

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

while we also want to indent if body like this:

```swift
if anotherVeryLongVariableName
     .veryLongPropertyName {
    aaa
}
```

Then, how should we indent this when the cursor is before `@`?

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
```
or
```swift
var x = foo
  .bar {
      @abc var x = 1
      x
  }
```

Both are syntactically correct code. We cannot handle this case properly. This is also a future work.

## Hacking

To build the package locally, run `make package`. You need [Cask](https://github.com/cask/cask).

To install the built package, run `make install`.

To run tests, run `make check`.

For other commands, run `make help`.

## Related projects

- [Official swift-mode.el by Apple](https://github.com/apple/swift/blob/master/utils/swift-mode.el) Seems still in very early stage for now. We cannot contribute to it due to the license incompatibility.
- [company-sourcekit](https://github.com/nathankot/company-sourcekit) Completion for Swift projects via SourceKit with the help of SourceKitten.
- [flycheck-swift](https://github.com/swift-emacs/flycheck-swift) Flycheck extensions for Swift.

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

You can find a [full list of those people here](https://github.com/swift-emacs/swift-mode/graphs/contributors).

Thanks to [@purcell](https://github.com/purcell) (Steve Purcell) for advices on the code and arrangement for merging `swift3-mode` and `swift-mode`.

## License

GPLv3. See [COPYING][] for details. Copyright (C) 2014-2016 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[badge-travis]: https://travis-ci.org/swift-emacs/swift-mode.png?branch=master
[travis]: https://travis-ci.org/swift-emacs/swift-mode
[COPYING]: ./COPYING
