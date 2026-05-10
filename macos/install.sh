#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Only run this script if both gpg *and* the private key are available
if ! command -v gpg >/dev/null 2>&1; then
	echo "GPG is not installed. Please install it (e.g., brew install gpg)."
	exit 1
fi

if ! gpg --list-keys 5C9B334784343A49 >/dev/null 2>&1; then
	echo "GPG key with ID '5C9B334784343A49' is not available.  Please import it."
	echo "For example: gpg --import path/to/your/keyfile"
	exit 1
fi

CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"
echo "$CONFIG_HOME"

################
# stow'd files #
################

# Special-case zsh, authinfo, and pi because they do not follow the XDG spec
stow -d "$SCRIPT_DIR" -vt ~ zsh
stow -d "$SCRIPT_DIR/../common" -vt ~ authinfo

# pi keeps auth.json and sessions/ alongside settings.json under ~/.pi/agent.
# Pre-create the dir and use --no-folding so stow only symlinks settings.json
# instead of replacing the whole agent/ directory with a symlink.
mkdir -p ~/.pi/agent
stow -d "$SCRIPT_DIR/../common" -vt ~ --no-folding pi

# claude: settings.json + hooks live alongside plugin cache and other state.
# --no-folding so stow symlinks individual files, not the whole .claude dir.
mkdir -p ~/.claude/hooks
stow -d "$SCRIPT_DIR/../common" -vt ~ --no-folding claude

mkdir -p "${CONFIG_HOME}/doom"
stow -d "$SCRIPT_DIR/../common" -vt "${CONFIG_HOME}/doom" doom

for component in \
		ghostty \
		pip \
		skhd \
		tmux \
		yabai; do
	mkdir -p "${CONFIG_HOME}/${component}"
	stow -d "$SCRIPT_DIR" -vt "${CONFIG_HOME}/${component}" "${component}"
done

#######################
# end of stow'd files #
#######################

################
# tmux plugins #
################

# TODO: move this to XDG
if [ ! -d ~/.tmux/plugins/tpm ]; then
	git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
	# I stole this from https://github.com/tmux-plugins/tpm/issues/6
	TMUX_PLUGIN_MANAGER_PATH=~/.tmux/plugins/ ~/.tmux/plugins/tpm/bin/install_plugins
	# Remember that tmux plugins have different requirements, e.g. tmux-jump requires ruby to be installed!
	# TODO: figure out a way to specify (and install?) tmux-plugins requirements?
fi

#######################
# end of tmux plugins #
#######################

####################
# Install Brewfile #
####################

pushd macos
brew bundle
popd

###################
# End of Brewfile #
###################

#################
# yabai service #
#################

# Use launchctl directly — yabai --restart-service has the service label hardcoded
# and breaks when the tap changes (e.g. asmvik → koekeishiya). Glob for the plist
# instead so this is resilient to future renames.
yabai_plist=$(ls ~/Library/LaunchAgents/com.*.yabai.plist 2>/dev/null | head -1)
if [ -n "$yabai_plist" ]; then
	launchctl unload "$yabai_plist" 2>/dev/null || true
	launchctl load "$yabai_plist"
	echo "yabai service started from: $yabai_plist"
else
	echo "Warning: no yabai plist found in ~/Library/LaunchAgents — service not started."
fi

######################
# end yabai service  #
######################

##############
# Doom Emacs #
##############

if [ ! -d "${CONFIG_HOME}/emacs" ]; then
	git clone --depth=1 https://github.com/doomemacs/doomemacs "${CONFIG_HOME}/emacs"
	DOOMDIR="${CONFIG_HOME}/doom" "${CONFIG_HOME}/emacs/bin/doom" install --no-config --no-env
fi

#####################
# End of Doom Emacs #
#####################

############
# zsh area #
############
if [ ! -d ~/.oh-my-zsh ]; then
	sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh) --unattended --keep-zshrc"
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
  read -r first second <<< "$entry"
  if [[ -n $second ]]; then
    user="$first"
    plugin="$second"
  else
    plugin="$first"
  fi
  target_dir="$ZSH_CUSTOM/plugins/$plugin"
  if [ ! -d "$target_dir" ]; then
    git clone "https://github.com/$user/$plugin" "$target_dir"
  fi
done

theme_user=romkatv
theme_name=powerlevel10k
theme_dir="$ZSH_CUSTOM/themes/$theme_name"
if [ ! -d "$theme_dir" ]; then
  git clone --depth=1 "https://github.com/$theme_user/$theme_name.git" "$theme_dir"
fi

###################
# end of zsh area #
###################

############################
# Overwrite macos defaults #
############################
defaults write com.apple.dock appswitcher-all-displays -bool true
defaults write com.apple.dock autohide -bool true
defaults write com.apple.screencapture location -string "$HOME/Desktop"
defaults write com.apple.screencapture disable-shadow -bool true
defaults write com.apple.screencapture type -string "png"
defaults write com.apple.Finder AppleShowAllFiles -bool true

###################################
# End of Overwrite macos defaults #
###################################

########
# Misc #
########

# fzf has been such an integral part of the toolset
if [ ! -d ~/.fzf ]; then
	git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
	~/.fzf/install --key-bindings --completion --update-rc --no-bash --no-fish
fi

###############
# end of misc #
###############
