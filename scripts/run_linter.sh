#!/bin/sh

# Run linter.  Used in Makefile.

./scripts/invoke_eldev.sh emacs --version || exit 1
find . -name '*.elc' -exec rm '{}' '+' || exit 1
rm -f *-autoloads.el || exit 1
./scripts/invoke_eldev.sh compile --set=all
find ./*.el test/*.el '!' -name '*autoloads.el' -exec \
     ./scripts/invoke_eldev.sh lint doc re elisp '{}' '+'
