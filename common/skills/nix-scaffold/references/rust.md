# Rust

## devShell packages

```nix
[ pkgs.rustup pkgs.cargo pkgs.rustfmt pkgs.clippy pkgs.rust-analyzer ]
```

Or if pinning a toolchain via `fenix` is preferred, note it as a comment but
default to rustup for simplicity.

## packages.default derivation

```nix
pkgs.rustPlatform.buildRustPackage {
  pname = "<project-name>";
  version = "0.1.0";
  src = ./.;
  cargoLock.lockFile = ./Cargo.lock;
}
```

Note: this requires `Cargo.lock` to exist. Add a TODO comment if scaffolding
before `cargo init` has been run.

## Justfile commands

```
build:   cargo build
test:    cargo test
run:     cargo run
fmt:     cargo fmt
lint:    cargo clippy
clean:   cargo clean
```

## .gitignore

```
/target
Cargo.lock   # remove this line if it's a binary crate (keep lock for binaries)
.direnv
result
```

(Keep `Cargo.lock` for binary crates; omit for libraries. Default: keep it,
add a comment.)
