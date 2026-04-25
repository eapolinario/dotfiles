# Simple helper targets for this dotfiles repository

# Path to baseline file that suppresses historical leaks
BASELINE := gitleaks.baseline

# Executable; override if you have gitleaks elsewhere, e.g. `make GITLEAKS=/path/to/gitleaks gitleaks`
GITLEAKS ?= gitleaks

.PHONY: gitleaks install-omarchy install-macos gitleaks-baseline-regen brewfile-update help
# Scan the repository for new secrets. Fails (non-zero exit status) if any leak
# that is *not* in $(BASELINE) is detected.
gitleaks:
	$(GITLEAKS) detect --source . --baseline-path $(BASELINE) --config .gitleaks.toml --redact

gitleaks-baseline-regen:
	$(GITLEAKS) detect --source . --config .gitleaks.toml --report-format json --report-path $(BASELINE)

install-omarchy:
	./omarchy/install.sh

install-macos:
	./macos/install.sh

brewfile-update:
	brew bundle dump --file ./macos/Brewfile --force

help:
	@echo "Available targets:"
	@echo "  gitleaks   – Run gitleaks with baseline $(BASELINE)"
	@echo "  install-omarchy  – Install dotfiles on Arch Linux / omarchy"
	@echo "  install-macos  – Install dotfiles on macOS"
	@echo "  brewfile-update – Update Brewfile from current Homebrew state"
