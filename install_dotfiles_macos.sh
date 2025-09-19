#!/bin/sh

set -eux # defensive bash programming.

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
echo $CONFIG_HOME

################
# stow'd files #
################

# Special-case zsh and authinfo because they do not follow the XDG spec
stow -vt ~ zsh
stow -vt ~ authinfo

for component in \
		doom \
		pip \
		skhd \
		tmux \
		yabai; do
	mkdir -p ${CONFIG_HOME}/${component}
	stow -vt ${CONFIG_HOME}/${component} ${component}
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

brew bundle

###################
# End of Brewfile #
###################

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
