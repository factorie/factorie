############################################################-*-Makefile-*-####
# Makefile for compiling the major mode of Emacs
##############################################################################
# $Id: Makefile 16681 2008-12-01 22:17:46Z nielsen $

##############################################################################
# Configuration

ROOT			 = .

SOURCE_DIR		 = $(ROOT)

##############################################################################
# Variables

# Emacs Lisp
ELISP_COMMAND		?= emacs
ELISP_OPTIONS		+= -batch -no-site-file
ELISP_OPTIONS           += -L $(ROOT) 
ELISP_OPTIONS		+= -f batch-byte-compile


ELISP_FILES		+= scala-mode
ELISP_FILES		+= scala-mode-auto
ELISP_FILES		+= scala-mode-inf
ELISP_FILES		+= scala-mode-indent
ELISP_FILES		+= scala-mode-navigation
ELISP_FILES		+= scala-mode-lib
ELISP_FILES		+= scala-mode-ui
ELISP_FILES		+= scala-mode-fontlock
ELISP_FILES		+= scala-mode-constants
ELISP_FILES		+= scala-mode-feature
ELISP_FILES		+= scala-mode-feature-electric
ELISP_FILES		+= scala-mode-feature-speedbar
ELISP_FILES		+= scala-mode-feature-tags

ELISP_SOURCES		+= $(ELISP_FILES:%=$(SOURCE_DIR)/%.el)

##############################################################################

RM			?= rm -f
TOUCH			?= touch

##############################################################################
# Commands

all: .latest-build

clean:
	$(RM) *.elc .latest-* autoloads.el

.PHONY: all
.PHONY: clean

##############################################################################
# Rules

.latest-build: $(ELISP_SOURCES)
	$(ELISP_COMMAND) $(ELISP_OPTIONS) $(ELISP_SOURCES)
	@$(TOUCH) $@

##############################################################################

autoloads: $(ELISP_SOURCES)
	emacs -batch -q --no-site-file --eval "(setq make-backup-files nil)" --eval "(setq generated-autoload-file (expand-file-name \"autoloads.el\"))" -f batch-update-autoloads `pwd`