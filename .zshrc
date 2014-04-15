# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Theme previews can be found at https://github.com/robbyrussell/oh-my-zsh/wiki/themes
ZSH_THEME="bureau"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to show in the command execution time stamp
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
#plugins=(git)

source $ZSH/oh-my-zsh.sh

#=========================================================================================
#
# User configuration
#
#=========================================================================================

TERM="xterm-256color"

#-----------------------------------------------------------------------------------------
# History
#-----------------------------------------------------------------------------------------

HISTSIZE=100000
HISTFILESIZE=200000
setopt HIST_IGNORE_DUPS

#-----------------------------------------------------------------------------------------
# Navigation
#-----------------------------------------------------------------------------------------

CDPATH=~

#-----------------------------------------------------------------------------------------
# Aliases
#-----------------------------------------------------------------------------------------

# common

alias cdd='cd ~/Downloads'
alias cdr='cd ~/dev/reactor'
alias cdu='cd ~/dev/uptimus'
alias df='df -h'
alias ll='ls -alp'
alias m='open -a MacVim'

# rails

alias log='tail -f ~/dev/uptimus/log/development.log'
alias p='psql'
alias r='rails'
alias rc='cd ~/dev/uptimus && rails console'
alias rs='cd ~/dev/uptimus && rails server'
alias sass='sass-convert --from css --to sass -R'

# ssh

alias staging="ssh devops@94.77.64.80"

# cap

alias pdeploy='git push && cap production deploy'
alias sdeploy='git push && cap staging deploy'
alias deploy='sdeploy'

# git

alias g='git'
alias ga='git add -A .'
alias gc='git commit -m'
alias gl="git log --graph --pretty=format:'%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr' --abbrev-commit"
alias gs='git status'
