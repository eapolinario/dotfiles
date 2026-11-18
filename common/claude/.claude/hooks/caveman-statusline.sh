#!/usr/bin/env bash
set -euo pipefail

# Stable wrapper — delegates to the latest dotted-numeric caveman cache version.
# Settings.json points here; plugin updates don't break the path.

version_is_newer() {
  local candidate="$1" current="$2" candidate_part current_part
  while [[ -n "$candidate" || -n "$current" ]]; do
    candidate_part="${candidate%%.*}"
    current_part="${current%%.*}"
    # Strip leading zeroes, then compare length/digits without integer overflow.
    candidate_part="${candidate_part#"${candidate_part%%[!0]*}"}"
    current_part="${current_part#"${current_part%%[!0]*}"}"
    if ((${#candidate_part} != ${#current_part})); then
      ((${#candidate_part} > ${#current_part}))
      return
    fi
    if [[ "$candidate_part" != "$current_part" ]]; then
      [[ "$candidate_part" > "$current_part" ]]
      return
    fi
    if [[ "$candidate" == *.* ]]; then candidate="${candidate#*.}"; else candidate=''; fi
    if [[ "$current" == *.* ]]; then current="${current#*.}"; else current=''; fi
  done
  return 1
}

select_script() {
  # Keep glob/tie/fallback ordering deterministic without changing the hook's locale.
  local LC_ALL=C
  local hook version newest_version='' fallback=''
  for hook in "${CLAUDE_CONFIG_DIR:-$HOME/.claude}/plugins/cache/caveman/caveman/"*/hooks/caveman-statusline.sh; do
    [[ -f "$hook" ]] || continue
    version="${hook%/hooks/caveman-statusline.sh}"
    version="${version##*/}"
    if [[ "$version" =~ ^[0-9]+(\.[0-9]+)*$ ]]; then
      if [[ -z "$newest_version" ]] || version_is_newer "$version" "$newest_version"; then
        script="$hook"
        newest_version="$version"
      fi
    elif [[ "$hook" > "$fallback" ]]; then
      # Compatibility for nonnumeric caches, not semver/prerelease ordering.
      fallback="$hook"
    fi
  done
  [[ -n "$script" ]] || script="$fallback"
}

script=''
select_script
[[ -n "$script" ]] || exit 0
exec bash "$script"
