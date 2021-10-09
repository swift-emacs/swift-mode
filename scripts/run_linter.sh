#!/bin/sh

# Run linter.  Used in Makefile.

./scripts/invoke_eldev.sh emacs --version || exit 1
find ./*.el test/*.el '!' -name '*autoloads.el' -exec ./scripts/invoke_eldev.sh lint doc re elisp '{}' '+'
