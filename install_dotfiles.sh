#!/bin/sh

set -eux # defensive bash programming.

CONFIG_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}"
echo $CONFIG_HOME

################
# stow'd files #
################

# Special-case zsh because that one is harder to force to read from `.config`
stow -vt ~ zsh

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

# zsh plugins
for PLUGIN in zsh-autosuggestions zsh-completions; do
	if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/$PLUGIN ]; then
		git clone https://github.com/zsh-users/$PLUGIN ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/$PLUGIN
	fi
done

if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/fast-syntax-highlighting ]; then
	git clone https://github.com/zdharma-continuum/fast-syntax-highlighting ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/fast-syntax-highlighting
fi
if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-256color ]; then
	git clone https://github.com/chrissicool/zsh-256color ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-256color
fi

if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/fzf-tab ]; then
	git clone https://github.com/Aloxaf/fzf-tab.git ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/fzf-tab
fi

if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/themes/powerlevel10k ]; then
	git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k
fi

###################
# end of zsh area #
###################

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
