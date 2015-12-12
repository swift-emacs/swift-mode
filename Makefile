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
EL          = $(DIST)/swift-mode-$(VERSION).el

.PHONY: all test deps install uninstall clean-all clean
all : deps $(DIST)

deps : $(PKG_DIR)
$(PKG_DIR) :
	$(CASK) install

test:
	$(CASK) exec ert-runner

install : $(DIST) $(USER_ELPA_D)
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(EL)")'

uninstall :
	rm -rf $(USER_ELPA_D)/swift-mode-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DIR)

clean :
	$(CASK) clean-elc
	rm -f *.elc
	rm -rf $(DIST)

$(DIST) :
	$(CASK) package

$(USER_ELPA_D) :
	mkdir -p $(USER_ELPA_D)
