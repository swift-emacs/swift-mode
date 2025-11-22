#!/bin/sh

# Run tests in Docker.  Used in Makefile.

if docker info --format '{{.SecurityOptions}}' | grep -q 'name=rootless'
then
    USER_OPT=""
else
    USER_OPT=--user="$(id -u):$(id -g)"
fi

for version in 30 29 28 27 26 25
do
    rm -f *.elc test/*.elc
    rm -f *-autoloads.el
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        $USER_OPT \
        --workdir="/src" \
        --env=ELDEV_DIR=/src/.eldev \
        --env=HOME=/tmp \
        silex/emacs:${version}-ci \
        bash -c "/src/scripts/run_test.sh" \
        || exit 1
done

echo "done"
