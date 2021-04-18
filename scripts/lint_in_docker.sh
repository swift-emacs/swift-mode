#!/bin/sh

# Run linter in Docker.  Used in Makefile.

for version in 24 25 26 27
do
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        --user "$(id -u):$(id -g)" \
        silex/emacs:${version} \
        bash -c \
        "cd /src && ELDEV_DIR=/src/.eldev HOME=/tmp ./scripts/run_linter.sh" \
        || exit 1
done

echo "done"
