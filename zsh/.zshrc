# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Make homebrew binaries show up first
export PATH="/opt/homebrew/bin:$PATH"

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:$HOME/.cargo/bin:/usr/local/bin:$PATH

# golang binaries should be in the path
export PATH=$HOME/go/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

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
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    # emacs
    extract
    fzf-tab
    git
    history
    kubectl
    python
    zsh-256color
    zsh-autosuggestions
    zsh-completions
    fast-syntax-highlighting
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

# Enable autocompletions
autoload -U compinit && compinit

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
alias gcms="git commit --signoff -m"
alias la="eza -la"

# Honestly, this is probably my favorite shell hack of all time. Full explanation in https://unix.stackexchange.com/a/25329/109848
alias watch='watch '
alias w5='watch -n5 '

# Assumes that the emacs server is already started
alias e="emacsclient --no-wait"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Setting fd as the default source for fzf. Follow symbolic links, do not exclude hidden files and .git
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

# To apply the command to CTRL-T as well
export FZF_CTRL_T_OPTS="--ansi --multi --preview 'bat --style=numbers --color=always --line-range :500 {}'"

export FZF_TAB_OPTS=(
  --expect='/'
  --color='hl:$(( $#headers == 0 ? 108 : 255 ))'
  --nth='2,3'
  --delimiter='\0'
  --tiebreak=begin -m --bind=tab:down,change:top,ctrl-space:toggle --cycle
  --query='$query'
  --header-lines='$#headers'
  --preview-window='40%'
)

# Context-aware completion using the '**' string
export FZF_COMPLETION_TRIGGER='**'

alias fzfp="fzf --ansi --multi --preview 'bat --style=numbers --color=always --line-range :500 {}'"

# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath' # remember to use single quote here!!!
zstyle ':fzf-tab:complete:systemctl:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'
# Catch-all requires an ad-hoc less script.
zstyle ':fzf-tab:complete:*:*' fzf-preview 'less ${(Q)realpath}'
export LESSOPEN='|~/.lessfilter.sh %s'

# # fzf-tab + tmux integration. Saw on this reddit thread: https://www.reddit.com/r/zsh/comments/jhcmkp/get_a_popup_completion_menu_with_fzftab_and_tmux/

# like normal z when used with arguments but displays an fzf prompt when used without.
# unalias z 2> /dev/null
# z() {
#     [ $# -gt 0 ] && zshz "$*" && return
#     cd "$(zshz -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "${*##-* }" | sed 's/^[0-9,.]* *//')"
# }

# Register the previous command in pet (https://github.com/knqyf263/pet).
# It requires pet to be installed.
function prev() {
  PREV=$(fc -lrn | head -n 1)
  sh -c "pet new `printf %q "$PREV"`"
}

# Kill a process by specifying a port it's listening to
function kill-by-port() {
  kill "$(lsof -t -i ":$1")"
}

# Change zsh auto-suggest highlight colors (as described in https://github.com/zsh-users/zsh-autosuggestions#configuration)
# export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=blue,bg=grey,bold,underline"
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#147494"

# As per https://github.com/ajeetdsouza/zoxide, need to initialize it in the shell
eval "$(zoxide init zsh)"
