#!/bin/sh

set -eux  # defensive bash programming.

# Get absolute path to directory for this script. This is useful if we want to invoke the script from outside the `/source_files` directory.
# I copied the solution from https://gist.github.com/tvlooy/cbfbdb111a4ebad8b93e#gistcomment-1650842.
function abs_script_dir_path {
    SOURCE=${BASH_SOURCE[0]}
    while [ -h "$SOURCE" ]; do
        DIR=$( cd -P $( dirname "$SOURCE") && pwd )
        SOURCE=$(readlink "$SOURCE")
        [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
    done
    DIR=$( cd -P $( dirname "$SOURCE" ) && pwd )
    echo $DIR
}

DIR=$(abs_script_dir_path $0)

# -f flag removes the target destination before creating the symbolic link. We do this to protect against failures (e.g. existent links).
# -n is to guard against creating a symbolic link inside a directory.
for DOTFILE in .tmux.conf .spacemacs .zshrc .zshenv .p10k.zsh; do
    ln -sfn $DIR/$DOTFILE ~/$DOTFILE
done;

################
# tmux plugins #
################

git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

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
for PLUGIN in zsh-autosuggestions zsh-completions zsh-syntax-highlighting fzf-tab; do
    if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/$PLUGIN ]; then
        git clone https://github.com/zsh-users/$PLUGIN ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/$PLUGIN
    fi
done;

if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-256color ]; then
    git clone https://github.com/chrissicool/zsh-256color ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-256color
fi

if [ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/themes/powerlevel10k ]; then
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k
fi

###################
# end of zsh area #
###################
