# Simple helper targets for this dotfiles repository

# Path to baseline file that suppresses historical leaks
BASELINE := gitleaks.baseline

# Executable; override if you have gitleaks elsewhere, e.g. `make GITLEAKS=/path/to/gitleaks gitleaks`
GITLEAKS ?= gitleaks

.PHONY: gitleaks install-linux install-macos
# Scan the repository for new secrets. Fails (non-zero exit status) if any leak
# that is *not* in $(BASELINE) is detected.
gitleaks:
	$(GITLEAKS) detect --source . --baseline-path $(BASELINE) --config .gitleaks.toml --redact

install-linux:
	./install_dotfiles_linux.sh

install-macos:
	./install_dotfiles_macos.sh

.PHONY: help
help:
	@echo "Available targets:"
	@echo "  gitleaks   – Run gitleaks with baseline $(BASELINE)"
