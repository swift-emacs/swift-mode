#!/bin/sh

# Run tests in Docker.  Used in Makefile.

for version in 28 27 26 25 24
do
    rm -f *.elc
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        --user "$(id -u):$(id -g)" \
        silex/emacs:${version} \
        bash -c \
        "cd /src && ELDEV_DIR=/src/.eldev HOME=/tmp ./scripts/run_test.sh" \
        || exit 1
done

echo "done"
