#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

DRY_RUN=0
while [[ $# -gt 0 ]]; do
	case "$1" in
	--dry-run | -n)
		DRY_RUN=1
		shift
		;;
	-h | --help)
		cat <<'USAGE'
Usage: install.sh [--dry-run]

  --dry-run, -n   Print actions instead of executing them. Skips the GPG
                  gate, runs `stow --simulate` instead of `stow`, replaces
                  `brew bundle` with `brew bundle list --all`, and skips
                  launchctl / defaults write / network clone steps. Used
                  by CI to validate the script end-to-end without mutating
                  the runner.
USAGE
		exit 0
		;;
	*)
		printf 'Unknown argument: %s\n' "$1" >&2
		exit 2
		;;
	esac
done

# Run a command, or print what would have run in dry-run mode.
run() {
	if [[ $DRY_RUN -eq 1 ]]; then
		printf '[dry-run] %s\n' "$*"
	else
		"$@"
	fi
}

# Run a stow invocation; in dry-run mode, force `--simulate` so the real
# binary still exercises path resolution and conflict detection without
# touching the filesystem.
run_stow() {
	if [[ $DRY_RUN -eq 1 ]]; then
		stow --simulate --verbose "$@"
	else
		stow "$@"
	fi
}

# Only run this script if both gpg *and* the private key are available
if [[ $DRY_RUN -eq 0 ]]; then
	if ! command -v gpg >/dev/null 2>&1; then
		echo "GPG is not installed. Please install it (e.g., brew install gpg)."
		exit 1
	fi

	if ! gpg --list-keys 5C9B334784343A49 >/dev/null 2>&1; then
		echo "GPG key with ID '5C9B334784343A49' is not available.  Please import it."
		echo "For example: gpg --import path/to/your/keyfile"
		exit 1
	fi
else
	echo "[dry-run] skipping GPG availability and key checks"
fi

CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"
echo "$CONFIG_HOME"

################
# stow'd files #
################

# Special-case zsh, authinfo, and pi because they do not follow the XDG spec
run_stow -d "$SCRIPT_DIR" -vt ~ zsh
run_stow -d "$SCRIPT_DIR/../common" -vt ~ authinfo

# pi keeps auth.json and sessions/ alongside settings.json under ~/.pi/agent.
# Pre-create the dir and use --no-folding so stow only symlinks settings.json
# instead of replacing the whole agent/ directory with a symlink.
mkdir -p ~/.pi/agent
run_stow -d "$SCRIPT_DIR/../common" -vt ~ --no-folding pi

# claude: settings.json + hooks live alongside plugin cache and other state.
# --no-folding so stow symlinks individual files, not the whole .claude dir.
mkdir -p ~/.claude/hooks
run_stow -d "$SCRIPT_DIR/../common" -vt ~ --no-folding claude

# copilot: global instructions + skills live alongside session state
# (config.json, logs/, session-store.db) and hand-made skill symlinks.
# --no-folding so stow never swaps ~/.copilot or its skills dir for a symlink.
mkdir -p ~/.copilot/skills
run_stow -d "$SCRIPT_DIR/../common" -vt ~ --no-folding copilot

mkdir -p "${CONFIG_HOME}/doom"
run_stow -d "$SCRIPT_DIR/../common" -vt "${CONFIG_HOME}/doom" doom

for component in \
	aerospace \
	ghostty \
	pip \
	tmux; do
	mkdir -p "${CONFIG_HOME}/${component}"
	run_stow -d "$SCRIPT_DIR" -vt "${CONFIG_HOME}/${component}" "${component}"
done

#######################
# end of stow'd files #
#######################

################
# tmux plugins #
################

# TODO: move this to XDG
if [ ! -d ~/.tmux/plugins/tpm ]; then
	run git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
	# I stole this from https://github.com/tmux-plugins/tpm/issues/6
	run env TMUX_PLUGIN_MANAGER_PATH="$HOME/.tmux/plugins/" ~/.tmux/plugins/tpm/bin/install_plugins
	# Remember that tmux plugins have different requirements, e.g. tmux-jump requires ruby to be installed!
	# TODO: figure out a way to specify (and install?) tmux-plugins requirements?
fi

#######################
# end of tmux plugins #
#######################

####################
# Install Brewfile #
####################

if [[ $DRY_RUN -eq 1 ]]; then
	echo "[dry-run] would run: brew bundle (file=$SCRIPT_DIR/Brewfile)"
	if command -v brew >/dev/null 2>&1; then
		echo "[dry-run] running: brew bundle list --all --file=$SCRIPT_DIR/Brewfile"
		brew bundle list --all --file="$SCRIPT_DIR/Brewfile" >/dev/null
	else
		echo "[dry-run]   (brew not available on this host; skipping list)"
	fi
else
	pushd "$SCRIPT_DIR" >/dev/null
	brew bundle
	popd >/dev/null
fi

###################
# End of Brewfile #
###################

############################
# Window manager migration #
############################

# AeroSpace owns the global window-management shortcuts. Stop the old services
# first so skhd cannot intercept AeroSpace bindings.
shopt -s nullglob
legacy_window_manager_plists=(
	"$HOME"/Library/LaunchAgents/com.*.skhd.plist
	"$HOME"/Library/LaunchAgents/com.*.yabai.plist
)
shopt -u nullglob

for plist in "${legacy_window_manager_plists[@]}"; do
	run launchctl unload "$plist" || true
done

for legacy_config in \
	"${CONFIG_HOME}/skhd/skhdrc" \
	"${CONFIG_HOME}/yabai/yabairc"; do
	if [[ -L $legacy_config ]]; then
		run rm "$legacy_config"
	fi
done

for formula in skhd yabai; do
	if brew list --formula "$formula" >/dev/null 2>&1; then
		run brew uninstall --formula "$formula"
	fi
done

run open -a AeroSpace

################################
# End window manager migration #
################################

######################
# sketchybar service #
######################

# `brew services restart` starts the agent if it is not running yet, so this is
# idempotent. sketchybar draws the focused-space indicator that macOS does not
# provide; yabai's external_bar setting reserves the room it occupies.
if command -v sketchybar >/dev/null 2>&1; then
	run brew services restart felixkratz/formulae/sketchybar
	echo "sketchybar service started."
else
	echo "Warning: sketchybar is not installed — status bar not started."
fi

#############################
# end of sketchybar service #
#############################

##############
# Doom Emacs #
##############

if [ ! -d "${CONFIG_HOME}/emacs" ]; then
	run git clone --depth=1 https://github.com/doomemacs/doomemacs "${CONFIG_HOME}/emacs"
	run env DOOMDIR="${CONFIG_HOME}/doom" "${CONFIG_HOME}/emacs/bin/doom" install --no-config --no-env
fi

#####################
# End of Doom Emacs #
#####################

############
# zsh area #
############
if [ ! -d ~/.oh-my-zsh ]; then
	if [[ $DRY_RUN -eq 1 ]]; then
		echo "[dry-run] would run: oh-my-zsh installer (curl|sh)"
	else
		sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh) --unattended --keep-zshrc"
	fi
fi

ZSH_CUSTOM=${ZSH_CUSTOM:-~/.oh-my-zsh/custom}

plugin_definitions=(
	"zsh-autosuggestions"
	"zsh-completions"
	"zdharma-continuum fast-syntax-highlighting"
	"chrissicool zsh-256color"
	"Aloxaf fzf-tab"
)

for entry in "${plugin_definitions[@]}"; do
	user="zsh-users"
	plugin=""
	read -r first second <<<"$entry"
	if [[ -n $second ]]; then
		user="$first"
		plugin="$second"
	else
		plugin="$first"
	fi
	target_dir="$ZSH_CUSTOM/plugins/$plugin"
	if [ ! -d "$target_dir" ]; then
		run git clone "https://github.com/$user/$plugin" "$target_dir"
	fi
done

theme_user=romkatv
theme_name=powerlevel10k
theme_dir="$ZSH_CUSTOM/themes/$theme_name"
if [ ! -d "$theme_dir" ]; then
	run git clone --depth=1 "https://github.com/$theme_user/$theme_name.git" "$theme_dir"
fi

###################
# end of zsh area #
###################

############################
# Overwrite macos defaults #
############################
run defaults write com.apple.dock appswitcher-all-displays -bool true
run defaults write com.apple.dock autohide -bool true
<<<<<<< HEAD
# Auto-hide the macOS menu bar so sketchybar owns the top of the screen instead
# of stacking below a second bar. macOS has no way to remove the menu bar
# outright; this is the "Always" option under Control Center > Menu Bar, and it
# still reveals on hover at the top edge.
run defaults write NSGlobalDomain _HIHideMenuBar -bool true
# Kill the Spaces / Mission Control switch animation (yabai cannot control this;
# window_animation_duration only affects yabai-managed window moves/resizes).
# The Dock keys cover Mission Control / app-exposé; the universalaccess key is
# what actually disables the Ctrl+arrow Space-to-Space slide on modern macOS
# (verified on macOS 26 / Tahoe). reduceMotion only takes effect after a
# logout/login.
=======
# Keep native Spaces animations disabled for the occasional macOS Space or
# Mission Control use outside AeroSpace. reduceMotion requires a logout/login.
>>>>>>> a128369 (Replace yabai with AeroSpace)
run defaults write com.apple.dock expose-animation-duration -float 0
run defaults write com.apple.dock workspaces-swoosh-animation-off -bool YES
run defaults write com.apple.universalaccess reduceMotion -bool true
# Disable standard window open/close/zoom animations globally.
run defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
run defaults write com.apple.screencapture location -string "$HOME/Desktop"
run defaults write com.apple.screencapture disable-shadow -bool true
run defaults write com.apple.screencapture type -string "png"
run defaults write com.apple.Finder AppleShowAllFiles -bool true
# Dock defaults above require a Dock restart to take effect; _HIHideMenuBar
# needs SystemUIServer restarted (a logout/login also works).
run killall Dock
run killall SystemUIServer

###################################
# End of Overwrite macos defaults #
###################################

########
# Misc #
########

# fzf has been such an integral part of the toolset
if [ ! -d ~/.fzf ]; then
	run git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
	run ~/.fzf/install --key-bindings --completion --update-rc --no-bash --no-fish
fi

###############
# end of misc #
###############
