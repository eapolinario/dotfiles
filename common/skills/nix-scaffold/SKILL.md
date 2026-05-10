---
name: nix-scaffold
description: >
  Scaffolds a new project with a Nix flake (flake.nix), Justfile, .envrc,
  .gitignore, and README.md. Use this skill whenever the user asks to create a
  new project, scaffold a project, start a new repo, or set up a project
  directory — especially when Nix, flakes, or Justfile are mentioned. Also
  trigger when the user says things like "new Go project", "start a Rust
  project", "bootstrap a Python repo", or any similar "fresh project" phrasing,
  even if they don't explicitly mention Nix or just.
---

# nix-scaffold

Generates a complete project scaffold for Eduardo's standard setup:
- `flake.nix` — nixpkgs-unstable, `devShells.default` + `packages.default`
- `Justfile` — `build`, `test`, `run`, `fmt`, `lint`, `clean` recipes
- `.envrc` — `use flake` for direnv auto-activation
- `.gitignore` — language-appropriate
- `README.md` — minimal project stub

Supported languages: **Rust**, **Go**, **Python**, **OCaml**, **Haskell**.

---

## Step 1 — Determine the language

If the user's message clearly names a language, use it. Otherwise ask:

> "Which language is this project? (Rust, Go, Python, OCaml, Haskell)"

Also confirm the project name if not given (used for directory names, flake
description, and README title). Default: the last path component they mentioned,
or ask.

---

## Step 2 — Generate the files

Create all files in the project directory (or current directory if none
specified). See the per-language templates in `references/`.

### flake.nix — common structure

```nix
{
  description = "<project-name>";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      pkgsFor = sys: import nixpkgs { system = sys; };
    in {
      devShells = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.mkShell {
            packages = <LANGUAGE_PACKAGES>;  # see references/
          };
        });

      packages = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = <LANGUAGE_BUILD_DERIVATION>;  # see references/
        });
    };
}
```

### Justfile — common structure

```just
set shell := ["bash", "-c"]

# List available recipes
default:
    @just --list

# Build the project
build:
    <LANGUAGE_BUILD_CMD>

# Run tests
test:
    <LANGUAGE_TEST_CMD>

# Run the project
run:
    <LANGUAGE_RUN_CMD>

# Format source code
fmt:
    <LANGUAGE_FMT_CMD>

# Lint / static analysis
lint:
    <LANGUAGE_LINT_CMD>

# Remove build artifacts
clean:
    <LANGUAGE_CLEAN_CMD>
```

Fill in `<LANGUAGE_*>` placeholders from `references/<language>.md`.

### .envrc

```bash
use flake
```

### README.md

```markdown
# <project-name>

> Short description here.

## Development

```sh
nix develop   # or: direnv allow
just build
just test
```
```

### .gitignore

Use the language-appropriate ignore rules from `references/<language>.md`.

---

## Step 3 — Output

Write each file. Then attempt to run `direnv allow` in the project directory:

```bash
cd <project-dir> && direnv allow
```

If the command succeeds, print:

```
✓ flake.nix
✓ Justfile
✓ .envrc
✓ .gitignore
✓ README.md
✓ direnv allow

Dev shell is ready — run `just build` to get started.
```

If the command fails or no shell access is available, print the summary without
the `direnv allow` line and add:

```
→ Run `direnv allow` or `nix develop` to enter the dev shell.
```

If the user is working in a specific directory, create files there. Otherwise
create them in the current working directory or a new subdirectory named after
the project.

---

## Notes

- If the user wants to deviate from any default (different nixpkgs channel,
  skip a file, add extra Justfile recipes), respect that — these are defaults,
  not mandates.
- If the language is not one of the five supported ones, generate a best-effort
  scaffold and note any gaps.
- The `packages.default` derivation should be a real (if minimal) derivation
  where easy (e.g. `rustPlatform.buildRustPackage`, `buildGoModule`). For
  languages where a Nix derivation is non-trivial (OCaml, Haskell, Python), use
  a `mkDerivation` stub with a TODO comment.

Read `references/<language>.md` for the exact packages, build commands, and
.gitignore contents for each supported language.
