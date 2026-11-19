#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_DIR="$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-gitleaks-history.XXXXXXXX")"
readonly REPO_DIR TEST_DIR
trap 'rm -r -- "$TEST_DIR"' EXIT

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

# Keep fixture Git configuration and commits independent of the user's setup.
export HOME="$TEST_DIR/home"
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
export GITLEAKS_DIFF="$TEST_DIR/diff.txt"
fixture="$TEST_DIR/repo"
mkdir -p -- "$HOME" "$fixture"
git -c init.defaultBranch=main init -q "$fixture"
git -C "$fixture" config user.name Fixture
git -C "$fixture" config user.email fixture@example.invalid
git -C "$fixture" config commit.gpgsign false
git -C "$fixture" config core.hooksPath /dev/null
printf 'fixture.bin diff=private-fixture\n' >"$fixture/.gitattributes"
printf '\0GITCRYPT\0fixture-ciphertext\n' >"$fixture/fixture.bin"
git -C "$fixture" add -- .gitattributes fixture.bin
git -C "$fixture" commit -qm fixture

printf '%s\n' '#!/usr/bin/env bash' 'printf "%s\n" fixture-cleartext-marker' >"$TEST_DIR/textconv.sh"
printf -v textconv_command 'bash %q' "$TEST_DIR/textconv.sh"
git -C "$fixture" config diff.private-fixture.textconv "$textconv_command"

# Observe real Git output with exactly the log options passed by the Makefile.
# No actual Gitleaks binary, private key, or credential is needed in this suite.
cat >"$TEST_DIR/gitleaks" <<'SCANNER'
#!/usr/bin/env bash
set -euo pipefail
[[ "${1:-}" == git && "${2:-}" == . ]] || exit 64
log_options=''
for argument in "$@"; do
  case "$argument" in
    --log-opts=*) log_options="${argument#*=}" ;;
  esac
done
[[ -n "$log_options" ]] || exit 64
read -r -a log_arguments <<<"$log_options"
git log -p "${log_arguments[@]}" >"$GITLEAKS_DIFF"
if grep -q fixture-cleartext-marker "$GITLEAKS_DIFF"; then
  printf 'Decrypted fixture text reached the scanner.\n' >&2
  exit 1
fi
SCANNER
chmod +x -- "$TEST_DIR/gitleaks"

# Prove the fixture exposes decrypted display text without the protective flag.
if (cd "$fixture" && "$TEST_DIR/gitleaks" git . --log-opts='--all --full-history') \
  >"$TEST_DIR/control.log" 2>&1; then
  fail 'textconv control unexpectedly passed'
fi
grep -q 'Decrypted fixture text reached the scanner' "$TEST_DIR/control.log" ||
  fail 'textconv control failed for an unrelated reason'

cp -- "$REPO_DIR/Makefile" "$fixture/Makefile"
printf -v scanner_command '%q' "$TEST_DIR/gitleaks"
for target in gitleaks gitleaks-baseline-regen; do
  if ! make --no-print-directory -C "$fixture" "GITLEAKS=$scanner_command" "$target" \
    >"$TEST_DIR/make.log" 2>&1; then
    printf '%s\n' "$(<"$TEST_DIR/make.log")" >&2
    fail "$target did not preserve raw committed content"
  fi
done
printf 'Gitleaks raw-history regression tests passed (both history targets).\n'
