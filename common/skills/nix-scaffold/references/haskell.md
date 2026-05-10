# Haskell

## devShell packages

```nix
[
  pkgs.ghc
  pkgs.cabal-install
  pkgs.haskell-language-server
  pkgs.haskellPackages.ormolu
  pkgs.haskellPackages.hlint
  pkgs.zlib   # commonly needed by cabal deps
]
```

For Stack-based projects, replace `cabal-install` with `pkgs.stack`.
Default: cabal.

## packages.default derivation

```nix
pkgs.haskellPackages.callCabal2nix "<project-name>" ./. {}
# TODO: if this errors on first run, try:
#   pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callCabal2nix ...)
```

## Justfile commands

```
build:   cabal build
test:    cabal test
run:     cabal run
fmt:     ormolu --mode inplace $(find . -name '*.hs')
lint:    hlint .
clean:   cabal clean
```

## .gitignore

```
dist-newstyle/
.stack-work/
*.hi
*.o
cabal.project.local
cabal.project.local~
.ghc.environment.*
.direnv
result
```
