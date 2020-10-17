#!/bin/sh

# Run tests.  Used in Makefile.

./scripts/invoke_eldev.sh emacs --version || exit 1
./scripts/invoke_eldev.sh emacs --batch -q \
  --eval "(add-to-list 'load-path \"$(readlink -f .)\")" \
  --eval "(add-to-list 'load-path \"$(readlink -f .)/test\")" \
  -f batch-byte-compile \
  ./*.el || exit 1
./scripts/invoke_eldev.sh emacs --batch -q \
  --eval "(add-to-list 'load-path \"$(readlink -f .)\")" \
  --eval "(add-to-list 'load-path \"$(readlink -f .)/test\")" \
  -l test/swift-mode-test.el \
  -f swift-mode:run-test
