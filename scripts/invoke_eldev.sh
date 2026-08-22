#!/bin/sh

# A little wrapper for eldev.  Used in Makefile.
# If ${ELDEV} is executable, use it.
# Otherwise, download the eldev to ${ELDEV_DIR}/bin/eldev if not exists, then
# execute it.

ELDEV_DIR=${ELDEV_DIR:-.eldev}

if command -v "${ELDEV}" > /dev/null
then
    "${ELDEV}" "$@"
elif command -v "${ELDEV_DIR}/bin/eldev" > /dev/null
then
    "${ELDEV_DIR}/bin/eldev" "$@"
else
    mkdir -p "${ELDEV_DIR}/bin" || exit 1
    curl -fsSL https://raw.github.com/doublep/eldev/master/bin/eldev -o "${ELDEV_DIR}/bin/eldev" || exit 1
    chmod a+x "${ELDEV_DIR}/bin/eldev" || exit 1
    "${ELDEV_DIR}/bin/eldev" "$@"
fi
