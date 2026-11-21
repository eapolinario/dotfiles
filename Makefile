# Simple helper targets for this dotfiles repository

# Path to baseline file that suppresses historical leaks
BASELINE := gitleaks.baseline

# Executable; override if you have gitleaks elsewhere, e.g. `make GITLEAKS=/path/to/gitleaks gitleaks`
GITLEAKS ?= gitleaks
# Scan committed content, not git-crypt's locally decrypted textconv output.
GITLEAKS_HISTORY_ARGS := git . --log-opts='--all --full-history --no-textconv'

SHFMT ?= shfmt
NVIM ?= nvim
EMACS ?= emacs
OMARCHY_SHELL_TESTS := $(wildcard omarchy/tests/*.sh)
OMARCHY_LUA_TESTS := $(wildcard omarchy/tests/*.lua)
OMARCHY_ELISP_TESTS := $(wildcard omarchy/tests/*.el)
OMARCHY_SHELL_FILES := omarchy/install.sh $(wildcard omarchy/hypr/.config/hypr/scripts/*.sh omarchy/tests/fixtures/*.sh) $(OMARCHY_SHELL_TESTS)

.PHONY: gitleaks install-omarchy install-macos gitleaks-baseline-regen brewfile-update help check-omarchy lint-omarchy test-omarchy
# Scan the repository for new secrets. Fails (non-zero exit status) if any leak
# that is *not* in $(BASELINE) is detected.
gitleaks:
	$(GITLEAKS) $(GITLEAKS_HISTORY_ARGS) --baseline-path $(BASELINE) --config .gitleaks.toml --redact

gitleaks-baseline-regen:
	$(GITLEAKS) $(GITLEAKS_HISTORY_ARGS) --config .gitleaks.toml --report-format json --report-path $(BASELINE)

install-omarchy:
	./omarchy/install.sh

check-omarchy: lint-omarchy test-omarchy

lint-omarchy:
	@set -e; for file in $(OMARCHY_SHELL_FILES); do bash -n "$$file"; done
	shellcheck -S warning --format=gcc $(OMARCHY_SHELL_FILES)
	"$(SHFMT)" -i 2 -ci -d $(OMARCHY_SHELL_FILES)
	systemd-analyze --user verify --man=no omarchy/systemd/.config/systemd/user/*.service
	systemd-analyze verify --man=no omarchy/systemd/system/*.service

test-omarchy:
	@set -e; for file in $(OMARCHY_SHELL_TESTS); do NVIM="$(NVIM)" bash "$$file"; done
	@set -e; for file in $(OMARCHY_LUA_TESTS); do "$(NVIM)" --headless -u NONE -i NONE -l "$$file"; done
	@set -e; for file in $(OMARCHY_ELISP_TESTS); do EMACS="$(EMACS)" bash omarchy/tests/fixtures/run-doom.sh "$(CURDIR)/$$file"; done

install-macos:
	./macos/install.sh

brewfile-update:
	brew bundle dump --file ./macos/Brewfile --force

help:
	@echo "Available targets:"
	@echo "  gitleaks   – Run gitleaks with baseline $(BASELINE)"
	@echo "  install-omarchy  – Install dotfiles on Arch Linux / omarchy"
	@echo "  check-omarchy    – Lint and run isolated Linux/shared-config regressions"
	@echo "  test-omarchy     – Run isolated Bash, Lua, and Emacs regression suites"
	@echo "  install-macos  – Install dotfiles on macOS"
	@echo "  brewfile-update – Update Brewfile from current Homebrew state"
