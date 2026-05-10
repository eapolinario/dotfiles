# OCaml

## devShell packages

```nix
[
  pkgs.ocaml
  pkgs.opam
  pkgs.ocamlPackages.ocaml-lsp
  pkgs.ocamlPackages.ocamlformat
  pkgs.ocamlPackages.dune_3
  pkgs.ocamlPackages.odoc
]
```

## packages.default derivation

OCaml/Dune projects are best built via opam2nix or dream2nix, which is
non-trivial to scaffold generically. Use a stub:

```nix
pkgs.stdenv.mkDerivation {
  pname = "<project-name>";
  version = "0.1.0";
  src = ./.;
  buildInputs = [ pkgs.ocaml pkgs.ocamlPackages.dune_3 ];
  buildPhase = "dune build";
  installPhase = "dune install";
  # TODO: replace with ocamlPackages.buildDunePackage for proper packaging
}
```

## Justfile commands

```
build:   dune build
test:    dune test
run:     dune exec ./<project-name>.exe
fmt:     ocamlformat --inplace **/*.ml **/*.mli
lint:    dune build @check
clean:   dune clean
```

Note: adjust the `run` target binary name to match the actual executable defined
in the `dune` file.

## .gitignore

```
_build/
*.install
*.merlin
.merlin
*.byte
*.native
*.annot
.direnv
result
```
