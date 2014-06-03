CWD          = $(shell pwd)
DIST         = $(CWD)/dist
DOC          = $(CWD)/doc
SKELETONS    = $(CWD)/project-skeletons
SCRIPT       = $(CWD)/script
GIT_DIR      = $(CWD)/.git
CASK        ?= cask
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_EMACS_D = ~/.emacs.d
USER_ELPA_D  = $(USER_EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
DOC_ORG      = $(DOC)/__PACKAGE-NAME__.org
DOC_TEXI     = $(DOC)/__PACKAGE-NAME__.texi
INFO_MANUAL  = $(DOC)/__PACKAGE-NAME__.info
PACKAGE_TAR  = $(DIST)/__PACKAGE-NAME__-$(VERSION).tar

PRECOMMIT_SRC  = $(SCRIPT)/pre-commit.sh
PRECOMMIT_HOOK = $(GIT_DIR)/hooks/pre-commit

.PHONY: all
all : deps

.PHONY: deps
deps : $(PKG_DIR)
$(PKG_DIR) :
	$(CASK) install

# Add precommit hook to run tests before committing.
.PHONY: hook
hook : $(PRECOMMIT_HOOK)
$(PRECOMMIT_HOOK) :
	ln -s $(PRECOMMIT_SRC) $(PRECOMMIT_HOOK)
	chmod +x $(PRECOMMIT_HOOK)

.PHONY: check
check : deps
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

.PHONY: install
install : $(DIST) $(USER_ELPA_D)
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(PACKAGE_TAR)")'

.PHONY: uninstall
uninstall :
	rm -rf $(USER_ELPA_D)/__PACKAGE-NAME__-*

.PHONY: reinstall
reinstall : clean uninstall install

.PHONY: clean-all
clean-all : clean
	rm -rf $(PKG_DIR)

.PHONY: clean
clean :
	cask clean-elc
	rm -rf $(DIST)
	rm -f $(DOC_TEXI)
	rm -f $(INFO_MANUAL)

$(DIST) : $(INFO_MANUAL)
	$(CASK) package

$(INFO_MANUAL) : deps $(DOC_ORG)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	-l org -l ox-texinfo \
	--file=$(DOC_ORG) -f org-texinfo-export-to-info

$(USER_ELPA_D) :
	mkdir -p $(USER_ELPA_D)
