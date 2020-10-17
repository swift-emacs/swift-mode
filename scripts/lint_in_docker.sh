#!/bin/sh

# Run linter in Docker.  Used in Makefile.

for version in 24.4 24.5 25.1 25.2 25.3 26.1 26.2 26.3 27.1
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
