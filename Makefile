# One-command checks for org-other-agenda.
#
# The default Org is the copy bundled with Emacs.  When testing against a
# newer Org checkout, pass its lisp directory explicitly, for example:
#
#     make ORG=/path/to/org/lisp check

EMACS ?= emacs
TEXTUI ?= ../textui
ORG ?=

ELISP_SOURCES := org-other-agenda-day.el org-other-agenda.el
ELISP_BYTECODES := $(ELISP_SOURCES:.el=.elc)
TEST_FILE := test/org-other-agenda-test.el

EMACS_LOAD_PATH := -L "$(TEXTUI)" -L .
ifneq ($(strip $(ORG)),)
EMACS_LOAD_PATH += -L "$(ORG)"
endif

.PHONY: test compile checkdoc check clean

test:
	$(EMACS) -Q --batch $(EMACS_LOAD_PATH) \
		-l $(TEST_FILE) \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch $(EMACS_LOAD_PATH) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(ELISP_SOURCES)

# checkdoc-file reports defects but does not set an error status; inspect its
# stable file:line diagnostic output in the shell.
checkdoc:
	@status=0; \
	for file in $(ELISP_SOURCES); do \
		output=$$($(EMACS) -Q --batch $(EMACS_LOAD_PATH) \
			--eval "(require 'checkdoc)" \
			--eval "(checkdoc-file \"$$file\")" 2>&1); \
		command_status=$$?; \
		if test -n "$$output"; then printf '%s\n' "$$output"; fi; \
		if test $$command_status -ne 0; then \
			status=$$command_status; \
		elif printf '%s\n' "$$output" | grep -Eq '^[^ ]*\.el:[0-9]'; then \
			status=1; \
		fi; \
	done; \
	exit $$status

check: compile checkdoc test

clean:
	$(RM) $(ELISP_BYTECODES)
