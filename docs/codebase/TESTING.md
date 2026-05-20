# Testing Patterns

## Core Sections (Required)

### 1) Test Stack and Commands

- Primary test framework: [TODO] No dedicated test framework configuration was identified in the files reviewed
- Assertion/mocking tools: [TODO]
- Commands:

```bash
bash -n path/to/script.sh
shellcheck path/to/script.sh
shfmt -d path/to/script.sh
./omarchy/install.sh --dry-run
cd macos && brew bundle check
cd nixos && make check
make gitleaks
```

### 2) Test Layout

- Test file placement pattern: No dedicated `test/`, `tests/`, `__tests__/`, `spec/`, or `*.bats` files were found in the repository tree reviewed
- Naming convention: [TODO] No repo-wide automated test filename pattern is present
- Setup files and where they run: CI setup currently lives in `.github/workflows/`; local validation guidance lives in `AGENTS.md` and the `Makefile` / `nixos/Makefile`

### 3) Test Scope Matrix

| Scope | Covered? | Typical target | Notes |
|-------|----------|----------------|-------|
| Unit | No | [TODO] | No unit test files or unit test runner config were found |
| Integration | No | [TODO] | Repo uses validation commands instead of integration test suites; examples include `brew bundle check`, `nix flake check`, host evaluation, and installer dry-runs |
| E2E | No | [TODO] | No end-to-end or smoke-test framework was found |

### 4) Mocking and Isolation Strategy

- Main mocking approach: [TODO] No mocking layer or fixture setup was identified
- Isolation guarantees: Validation is mostly command-based and platform-scoped rather than mock-based
- Common failure mode in tests: [TODO] No test suite is present; likely failure surfaces are installer regressions, Nix evaluation issues, and secret-handling mismatches

### 5) Coverage and Quality Signals

- Coverage tool + threshold: [TODO]
- Current reported coverage: [TODO]
- Known gaps/flaky areas: No automated installer smoke tests are present, Omarchy dry-run is not in CI yet, and the planning file explicitly proposes future Bats smoke tests and a CI Omarchy dry-run check

### 6) Evidence

- `AGENTS.md`
- `Makefile`
- `nixos/Makefile`
- `.github/workflows/shellcheck.yml`
- `.github/workflows/gitleaks.yml`
- `.github/workflows/nixos-eval.yml`
- `omarchy/install.sh`
- `macos/Brewfile`
- `plans/repo-improvements-2026-04-25.md`
