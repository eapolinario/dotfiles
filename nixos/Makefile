NIX       ?= nix
FLAKE_DIR := $(shell pwd)
FLAKE_REF ?= .
CI_METADATA_ATTR ?= $(FLAKE_REF)\#lib.ciMetadata

# Stderr is suppressed so that a missing/unavailable flake does not clutter make output;
# the _require-host/_require-hosts guards below provide actionable errors when the values
# are empty.  Remove 2>/dev/null temporarily to see raw nix evaluation errors.
DEFAULT_HOST = $(shell $(NIX) eval --raw --apply 'meta: if meta.defaultBuildHost == null then "" else meta.defaultBuildHost.name' $(CI_METADATA_ATTR) 2>/dev/null)
DEFAULT_HOSTS = $(shell $(NIX) eval --raw $(CI_METADATA_ATTR).evalHostsText 2>/dev/null)
DEFAULT_BUILD_RUNNER = $(shell $(NIX) eval --raw --apply 'meta: if meta.defaultBuildHost == null then "" else meta.defaultBuildHost.runner' $(CI_METADATA_ATTR) 2>/dev/null)

# Local defaults are derived from flake-exported host metadata, but can still be overridden.
HOST      ?= $(DEFAULT_HOST)
HOSTS     ?= $(DEFAULT_HOSTS)

_require-host:
	@[ -n "$(HOST)" ] || { printf 'Error: HOST is unset — either pass HOST=<name> or mark a host with ci.build = true in your flake.\n' >&2; exit 1; }

_require-hosts:
	@[ -n "$(HOSTS)" ] || { printf 'Error: HOSTS is unset — either pass HOSTS="h1 h2" or mark hosts with ci.eval = true in your flake.\n' >&2; exit 1; }

.PHONY: switch deploy fmt fmt-check gc update-lock flake-check eval-hosts build-host check print-default-host print-eval-hosts print-build-runner _require-host _require-hosts help

## switch: rebuild and switch the flake on this machine
switch: _require-host
	sudo nixos-rebuild switch --flake .#$(HOST)

## deploy: full nixos-anywhere install (first-time, wipes disk)
deploy: _require-host
	bash $(FLAKE_DIR)/deploy.sh $(HOST) $(TARGET)

## gc: run nix garbage collection
gc:
	nix-collect-garbage -d

## fmt: format all nix files
fmt:
	$(NIX) fmt $(FLAKE_DIR)

## fmt-check: verify all nix files are already formatted (fails on drift)
fmt-check:
	$(NIX) fmt $(FLAKE_DIR) -- --ci

## update-lock: update flake.lock inputs
update-lock:
	$(NIX) flake update $(FLAKE_REF)

## flake-check: run nix flake checks for this flake
flake-check:
	$(NIX) flake check $(FLAKE_REF)

## eval-hosts: evaluate declared NixOS hosts without building them
eval-hosts: _require-hosts
	@set -e; \
	for host in $(HOSTS); do \
		printf 'Evaluating nixosConfiguration %s...\n' "$$host"; \
		$(NIX) eval "$(FLAKE_REF)#nixosConfigurations.$$host.config.system.build.toplevel.drvPath" >/dev/null; \
	done

## build-host: build the selected NixOS host toplevel
build-host: _require-host
	$(NIX) build "$(FLAKE_REF)#nixosConfigurations.$(HOST).config.system.build.toplevel"

## check: run local validation for the NixOS flake
check: flake-check eval-hosts

## print-default-host: print the default build host derived from flake metadata
print-default-host:
	@printf '%s\n' "$(DEFAULT_HOST)"

## print-eval-hosts: print the default evaluation host list derived from flake metadata
print-eval-hosts:
	@printf '%s\n' "$(DEFAULT_HOSTS)"

## print-build-runner: print the default CI build runner derived from flake metadata
print-build-runner:
	@printf '%s\n' "$(DEFAULT_BUILD_RUNNER)"

## help: show this help
help:
	@grep -E '^## ' Makefile | sed 's/## //'
