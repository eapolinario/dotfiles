HOST     ?= fusion-vm
FLAKE_DIR = $(shell pwd)

.PHONY: switch deploy fmt gc help

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
	nix fmt $(FLAKE_DIR)

## help: show this help
help:
	@grep -E '^## ' Makefile | sed 's/## //'
