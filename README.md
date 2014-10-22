[![License GPL 3][badge-license]][copying]
[![Build Status][badge-travis]][travis]

# swift-mode

## Summary

Major-mode for Apple's [Swift programming language][swift]. Provides:

- syntax highlighting
- indentation
- code navigation with [imenu][]
- automatic syntax checking with [flycheck][] (disabled by default)

This is currently at an early stage of development and there's plenty of work to
do. Check the issue tracker.

Requires Emacs 24 or later.

## Installing

`swift-mode` can be installed using Emacs' built-in package manager or from
source. You can also install [flycheck][] if you want syntax checking.

### package.el

#### MELPA

You can install a snapshot version of `swift-mode` from the [MELPA][]
repository. The version of `swift-mode` there will always be up-to-date, but it
might be unstable (albeit rarely).

You can add MELPA to the list of `package.el` repositories like this:

```el
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
```

#### MELPA Stable

You can install the last stable version of `swift-mode` from the
[MELPA Stable][] repository.

You can add MELPA Stable to the list of `package.el` repositories like this:

```el
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
```

***

The package installation is as easy as:

```
M-x package-install swift-mode
```

If you'd like to get on-the-fly syntax checking you should install
[flycheck][] as well:

```
M-x package-install flycheck
```

And enable `flycheck` checker for `swift`:

```
(add-to-list 'flycheck-checkers 'swift)
```

### Manual

You will need `make` and [Cask][] to build the project.

```
cd swift-mode
make && make install
```

This will install `swift-mode` via `package.el` locally.

You can also install `swift-mode` the old-school way by simply dropping it
somewhere on your `load-path`.

```el
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'swift-mode)
```

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## Acknowledgements

The REPL code is based on [js-comint][].

Thanks to the following users for their contributions:

- [@ap4y](https://github.com/ap4y) (Arthur Evstifeev)
- [@bbatsov](https://github.com/bbatsov) (Bozhidar Batsov)
- [@syohex](https://github.com/syohex) (Syohei Yoshida)

## License

See [COPYING][]. Copyright (c) 2014 Chris Barrett.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[badge-travis]: https://travis-ci.org/chrisbarrett/swift-mode.png?branch=master
[travis]: https://travis-ci.org/chrisbarrett/swift-mode
[COPYING]: https://github.com/chrisbarrett/swift-mode/blob/master/COPYING
[CONTRIBUTING]: https://github.com/chrisbarrett/swift-mode/blob/master/CONTRIBUTING.md
[swift]: https://developer.apple.com/swift/
[cask]: https://github.com/cask/cask
[rust-mode]: https://github.com/mozilla/rust/tree/master/src/etc/emacs
[melpa]: http://melpa.milkbox.net
[melpa stable]: http://melpa-stable.milkbox.net
[imenu]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Imenu.html
[flycheck]: http://flycheck.readthedocs.org/en/latest/
[js-comint]: http://js-comint-el.sourceforge.net/
