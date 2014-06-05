CWD          = $(shell pwd)
DIST         = $(CWD)/dist
CASK        ?= cask
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_EMACS_D = ~/.emacs.d
USER_ELPA_D  = $(USER_EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
TAR          = $(DIST)/swift-mode-$(VERSION).tar

.PHONY: all
all : deps $(DIST)

.PHONY: deps
deps : $(PKG_DIR)
$(PKG_DIR) :
	$(CASK) install

.PHONY: check
check : deps
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

.PHONY: install
install : $(DIST) $(USER_ELPA_D)
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(TAR)")'

.PHONY: uninstall
uninstall :
	rm -rf $(USER_ELPA_D)/swift-mode-*

.PHONY: reinstall
reinstall : clean uninstall install

.PHONY: clean-all
clean-all : clean
	rm -rf $(PKG_DIR)

.PHONY: clean
clean :
	$(CASK) clean-elc
	rm -f *.elc
	rm -rf $(DIST)

$(DIST) :
	$(CASK) package

$(USER_ELPA_D) :
	mkdir -p $(USER_ELPA_D)
