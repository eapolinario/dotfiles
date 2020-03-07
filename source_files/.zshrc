# For pkg-config to find zlib you may need to set:
export PKG_CONFIG_PATH="${PKG_CONFIG_PATH} /usr/local/opt/zlib/lib/pkgconfig"

# Path to your oh-my-zsh installation.
export ZSH="/Users/eapolinario/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="robbyrussell"
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=14

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

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

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

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
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias gp="git pull"
alias gc="git checkout"
alias gmm="git merge origin master"
alias fixspacemacs="cd ~/.emacs.d && git pull --rebase; find ~/.emacs.d/elpa/2*/develop/org-plus-contrib* -name '*.elc' -delete"  # update spacemacs (copied from https://github.com/syl20bnr/spacemacs/issues/11801)

# As per the documentation, we should the syntax highlighting plugin only at the end of the .zshrc file
source /Users/eapolinario/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source '/Users/eapolinario/repos/awsaccess/awsaccess2.sh' # awsaccess
source '/Users/eapolinario/repos/awsaccess/oktaawsaccess.sh' # oktaawsaccess
export PS1="\$(ps1_mfa_context)$PS1" # awsaccess
PATH=$PATH:/Users/eapolinario/.lyftkube-bin
export GOBIN=$GOPATH/bin
export PATH="/usr/local/opt/llvm/bin:/usr/local/opt/go@1.13/bin:$PATH"
# The following path may vary based on where you have checked out the repo
export FAB_HOME=$HOME/repos/hacktools/fab
alias fab="$FAB_HOME/fab -f $FAB_HOME/fabfile"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
