# Go

## devShell packages

```nix
[ pkgs.go pkgs.gopls pkgs.golangci-lint pkgs.gotools ]
```

## packages.default derivation

```nix
pkgs.buildGoModule {
  pname = "<project-name>";
  version = "0.1.0";
  src = ./.;
  vendorHash = pkgs.lib.fakeHash; # replace after first build
}
```

Note: `vendorHash` must be updated after running `go mod vendor`. Add a TODO
comment in the generated flake.

## Justfile commands

```
build:   go build ./...
test:    go test ./...
run:     go run ./...
fmt:     gofmt -w .
lint:    golangci-lint run
clean:   go clean ./...
```

## .gitignore

```
# Binaries
*.exe
*.exe~
*.dll
*.so
*.dylib
<project-name>

# Test artifacts
*.test
*.out

# Go workspace
go.work.sum

.direnv
result
```
