# swift-mode

## Summary

Major-mode for Apple's Swift programming language. Currently provides:

- syntax highlighting
- indentation

This is currently at an early stage of development and there's plenty of work to
do. Check the issue tracker.

Requires Emacs 24 or later.

## Installing

### package.el

#### MELPA

You can install a snapshot version of `swift-mode` from the
[MELPA](http://melpa.milkbox.net) repository. The version of
`switf-mode` there will always be up-to-date, but it might be unstable
(albeit rarely).

#### MELPA Stable

You can install the last stable version of `swift-mode` from the
[MELPA Stable](http://melpa-stable.milkbox.net) repository.

### Manual

You will need Emacs 24+, `make` and [Cask](https://github.com/cask/cask) to
build the project.

    cd swift-mode
    make && make install

## Contributing

Yes, please do! See CONTRIBUTING.md for guidelines.

## Acknowledgements

The indentation code is heavily based on
[rust-mode](https://github.com/mozilla/rust/tree/master/src/etc/emacs)'s
implementation.

Thanks to the following users for their contributions:

- [@syohex](https://github.com/syohex) (Syohei Yoshida)

## License

See COPYING. Copyright (c) 2014 Chris Barrett.
