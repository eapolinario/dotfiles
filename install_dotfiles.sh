#!/bin/sh

set -eux  # defensive bash programming.

################
# stow'd files #
################
for d in doom-emacs tmux; do
    stow -nvt ~ $d
done

#######################
# end of stow'd files #
#######################

################
# tmux plugins #
################

if [ ! -d ~/.tmux/plugins/tpm ]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# TODO do I have to clone plugins or can I rely on https://github.com/tmux-plugins/tpm#installing-plugins ?

#######################
# end of tmux plugins #
#######################

############
# zsh area #
############
if [ ! -d ~/.oh-my-zsh ]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh) --unattended --keep-zshrc"
fi

# zsh plugins
for PLUGIN in zsh-autosuggestions zsh-completions zsh-syntax-highlighting; do
    if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/$PLUGIN ]; then
        git clone https://github.com/zsh-users/$PLUGIN ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/$PLUGIN
    fi
done;

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
