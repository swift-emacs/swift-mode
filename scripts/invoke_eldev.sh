#!/bin/sh

# A little wrapper for eldev.  Used in Makefile.
# If ${ELDEV} is executable, use it.
# Otherwise, download the eldev to .eldev/bin/eldev if not exists, then
# execute it.

if command -v "${ELDEV}" > /dev/null
then
    "${ELDEV}" "$@"
elif command -v .eldev/bin/eldev > /dev/null
then
    .eldev/bin/eldev "$@"
else
    mkdir -p ./.eldev/bin || exit 1
    curl -fsSL https://raw.github.com/doublep/eldev/master/bin/eldev -o .eldev/bin/eldev || exit 1
    chmod a+x .eldev/bin/eldev || exit 1
    .eldev/bin/eldev "$@"
fi
