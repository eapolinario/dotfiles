# Python

## devShell packages

```nix
[ pkgs.uv pkgs.ruff pkgs.pyright ]
```

No need for `python3` directly — uv manages the Python interpreter and virtualenv.

Note in the README: run `uv sync` after entering the shell to install dependencies.

## packages.default derivation

Python packaging via Nix is complex. Use a stub:

```nix
pkgs.python3Packages.buildPythonPackage {
  pname = "<project-name>";
  version = "0.1.0";
  src = ./.;
  # TODO: add propagatedBuildInputs for dependencies
}
```

Add a comment: "For development, prefer `nix develop` + uv."

## Justfile commands

```
build:   uv build
test:    uv run pytest
run:     uv run python -m <project_name>
fmt:     ruff format .
lint:    ruff check .
clean:   rm -rf dist/ build/ __pycache__ .pytest_cache .venv
```

## .gitignore

```
__pycache__/
*.py[cod]
*.pyo
.venv/
dist/
build/
*.egg-info/
.pytest_cache/
.ruff_cache/
.mypy_cache/
uv.lock
.direnv
result
```
