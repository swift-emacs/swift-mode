CASK ?= cask
EMACS ?= emacs
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRC = $(wildcard *.el)
PACKAGE = dist/swift-mode-$(VERSION).tar

.PHONY: help all deps package install test clean

help:
## Shows this message.
# Process this Makefile with following filters
#
# - Remove empty line.
# - Remove lien containing ## no-doc.
# - Remove after colon if the line is not a comment line.
# - Replace /^## / to "  ".
# - Remove other comment lines.
# - Insert newline before rules.
	@sed -e '/^\s*$$/d; /^[	.A-Z]/d; /## no-doc/d; s/^\([^#][^:]*\):.*/\1/; s/^## /  /; /^#/d; s/^[^ ]/\n&/' Makefile

all: package
## Builds the package.

$(PKG_DIR): ## no-doc
	$(CASK) install

deps: $(PKG_DIR)
## Installs the dependencies.

$(PACKAGE): $(SRC) deps ## no-doc
	rm -rf dist
	$(CASK) package

package: $(PACKAGE)
## Builds the package.

install: package
## Installs the package.
	$(CASK) exec $(EMACS) --batch \
	  -l package \
	  -f package-initialize \
	  --eval '(package-install-file "$(PACKAGE)")'

clean:
## Cleans the dist directory.
	rm -rf dist

check: deps
## Tests the package.
	$(CASK) exec $(EMACS) --batch -q \
	  --eval "(add-to-list 'load-path \""$(shell realpath .)"\")" \
	  -l swift-mode.el \
	  -l test/swift-mode-test-indent.el \
	  -f swift-mode:run-test:indent
