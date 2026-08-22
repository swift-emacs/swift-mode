ELDEV_DIR ?= .eldev
ELDEV ?= eldev

INVOKE_ELDEV = ELDEV_DIR="${ELDEV_DIR}" ELDEV="${ELDEV}" ./scripts/invoke_eldev.sh

.PHONY: help all deps package install clean test test_in_docker lint lint_in_docker

help:
## Shows this message.
# Process this Makefile with following filters
#
# - Remove empty line.
# - Remove line starting with whitespace, dot, or uppercase letters.
# - Remove line containing ## no-doc.
# - Remove after colon if the line is not a comment line.
# - Replace /^## / to "  ".
# - Remove other comment lines.
# - Insert empty line before rules (by printing the empty hold space).
	@sed -e '/^[[:space:]]*$$/d; /^[	_.A-Z]/d; /## no-doc/d; s/^\([^#][^:]*\):.*/\1/; s/^## /  /; /^#/d; /^[^ ]/{x;p;x;}' Makefile

all: package
## Builds the package.

deps:
## Installs the dependencies.
	${INVOKE_ELDEV} prepare

package:
## Builds the package.
	${INVOKE_ELDEV} package

install: package
## Installs the package.
	${INVOKE_ELDEV} emacs --batch \
	  -l package \
	  -f package-initialize \
	  -f package-refresh-contents \
	  --eval '(package-install-file "'"$$( ls dist/*.tar | sort | tail -n 1 )"'")'

clean:
## Cleans the dist directory, *.elc, and .eldev.
	${INVOKE_ELDEV} clean all

test:
## Tests the package.
	ELDEV_DIR="${ELDEV_DIR}" ELDEV="${ELDEV}" ./scripts/run_test.sh

test_in_docker:
## Run tests in Docker.
	./scripts/test_in_docker.sh

lint:
## Run linters.
	ELDEV_DIR="${ELDEV_DIR}" ELDEV="${ELDEV}" ./scripts/run_linter.sh

lint_in_docker:
## Run linter in Docker.
	./scripts/lint_in_docker.sh
