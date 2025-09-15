# Simple helper targets for this dotfiles repository
#
# TODO: remove later than 2025-09-15 15:12

# Path to baseline file that suppresses historical leaks
BASELINE := gitleaks.baseline

# Executable; override if you have gitleaks elsewhere, e.g. `make GITLEAKS=/path/to/gitleaks gitleaks`
GITLEAKS ?= gitleaks

.PHONY: gitleaks
# Scan the repository for new secrets. Fails (non-zero exit status) if any leak
# that is *not* in $(BASELINE) is detected.
gitleaks:
	$(GITLEAKS) detect --source . --baseline-path $(BASELINE) --redact

.PHONY: help
help:
	@echo "Available targets:"
	@echo "  gitleaks   – Run gitleaks with baseline $(BASELINE)"
