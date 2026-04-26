NIX       ?= nix
FLAKE_DIR := $(shell pwd)
FLAKE_REF ?= .
CI_METADATA_ATTR ?= $(FLAKE_REF)\#lib.ciMetadata

DEFAULT_HOST = $(shell $(NIX) eval --raw $(CI_METADATA_ATTR).defaultBuildHost.name)
DEFAULT_HOSTS = $(shell $(NIX) eval --raw $(CI_METADATA_ATTR).evalHostsText)
DEFAULT_BUILD_RUNNER = $(shell $(NIX) eval --raw $(CI_METADATA_ATTR).defaultBuildHost.runner)

# Local defaults are derived from flake-exported host metadata, but can still be overridden.
HOST      ?= $(DEFAULT_HOST)
HOSTS     ?= $(DEFAULT_HOSTS)

.PHONY: switch deploy fmt gc flake-check eval-hosts build-host check print-default-host print-eval-hosts print-build-runner help

## switch: rebuild and switch the flake on this machine
switch:
	sudo nixos-rebuild switch --flake .#$(HOST)

## deploy: full nixos-anywhere install (first-time, wipes disk)
deploy:
	bash $(FLAKE_DIR)/deploy.sh $(HOST) $(TARGET)

## gc: run nix garbage collection
gc:
	nix-collect-garbage -d

## fmt: format all nix files
fmt:
	$(NIX) fmt $(FLAKE_DIR)

## flake-check: run nix flake checks for this flake
flake-check:
	$(NIX) flake check $(FLAKE_REF)

## eval-hosts: evaluate declared NixOS hosts without building them
eval-hosts:
	@set -e; \
	for host in $(HOSTS); do \
		printf 'Evaluating nixosConfiguration %s...\n' "$$host"; \
		$(NIX) eval "$(FLAKE_REF)#nixosConfigurations.$$host.config.system.build.toplevel.drvPath" >/dev/null; \
	done

## build-host: build the selected NixOS host toplevel
build-host:
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
