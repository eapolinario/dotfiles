HOST      ?= fusion-vm
# Keep local defaults aligned with the declared host metadata in `nixos/flake.nix`.
HOSTS     ?= fusion-vm
NIX       ?= nix
FLAKE_DIR := $(shell pwd)
FLAKE_REF ?= .

.PHONY: switch deploy fmt gc flake-check eval-hosts build-host check help

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

## help: show this help
help:
	@grep -E '^## ' Makefile | sed 's/## //'
