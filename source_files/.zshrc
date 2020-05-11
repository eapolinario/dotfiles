# For pkg-config to find zlib you may need to set:
export PKG_CONFIG_PATH="${PKG_CONFIG_PATH} /usr/local/opt/zlib/lib/pkgconfig"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="robbyrussell"
ZSH_THEME="powerlevel10k/powerlevel10k"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=14

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    emacs
    git
    osx
    pyenv
    z
    zsh-256color
    zsh-autosuggestions
    zsh-completions
    zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# User configuration

# Trying out to get "infinite" history based on a similar bash concept (only that in bash it's a bit different: https://stackoverflow.com/questions/9457233/unlimited-bash-history)
HISTSIZE=999999999
SAVEHIST=$HISTSIZE

setopt HIST_IGNORE_ALL_DUPS # Do not enter command lines into the history list if they are duplicates of the previous event
setopt HIST_IGNORE_SPACE  # Remove command lines from the history list when the first character on the line is a space
setopt HIST_SAVE_NO_DUPS  # Don't write duplicate entries in the history file.
setopt AUTO_PUSHD # Make cd push the old directory onto the directory stack

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Enable autocompletions
autoload -U compinit && compinit

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
alias gp="git pull"
alias gc="git checkout"
alias gmm="git merge origin master"
alias fixspacemacs="cd ~/.emacs.d && git pull --rebase; find ~/.emacs.d/elpa/2*/develop/org-plus-contrib* -name '*.elc' -delete"  # update spacemacs (copied from https://github.com/syl20bnr/spacemacs/issues/11801)

# As per the documentation, we should the syntax highlighting plugin only at the end of the .zshrc file
source /Users/$(whoami)/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Lyft specific configs. I know, this is lame, but I never use that as my alias in my personal machines anyway. :-)
if [[ $(whoami) == "eapolinario" ]]; then
    source '/Users/eapolinario/repos/awsaccess/awsaccess2.sh' # awsaccess
    source '/Users/eapolinario/repos/awsaccess/oktaawsaccess.sh' # oktaawsaccess
    export PS1="\$(ps1_mfa_context)$PS1" # awsaccess
    PATH=$PATH:/Users/eapolinario/.lyftkube-bin
    export GOBIN=$GOPATH/bin
    export PATH="/usr/local/opt/llvm/bin:/usr/local/opt/go@1.14/bin:$PATH"
    # The following path may vary based on where you have checked out the repo
    export FAB_HOME=$HOME/repos/hacktools/fab
    alias fab="$FAB_HOME/fab -f $FAB_HOME/fabfile"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Setting fd as the default source for fzf. Follow symbolic links, do not exclude hidden files and .git
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
alias fzfp="fzf --ansi --multi --preview 'bat --style=numbers --color=always {} | head -n 100'"

# Experimenting with nix.
source $HOME/.nix-profile/etc/profile.d/nix.sh

# As per the documentation, we should the syntax highlighting plugin only at the end of the .zshrc file
source $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
