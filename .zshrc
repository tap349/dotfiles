# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Theme previews can be found at https://github.com/robbyrussell/oh-my-zsh/wiki/themes
ZSH_THEME="blinks"

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

#-----------------------------------------------------------------------------------------
#
# Environment variables
#
#-----------------------------------------------------------------------------------------

TERM="xterm-256color"

HISTSIZE=100000
HISTFILESIZE=200000
setopt HIST_IGNORE_DUPS

CDPATH=~

REACTOR=~/dev/reactor
UPTIMUS=~/dev/uptimus

#-----------------------------------------------------------------------------------------
#
# Aliases
#
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# dev
#-----------------------------------------------------------------------------------------

# cap

alias pdeploy='git push && cap production deploy'
alias sdeploy='git push && cap staging deploy'
alias deploy='sdeploy'

# git

alias g='git'
alias ga='git add -A .'
alias gl="git log --graph --pretty=format:'%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr' --abbrev-commit"
alias gs='git status'

# misc

alias guard='cd $UPTIMUS && guard'
alias log='tail -f $UPTIMUS/log/development.log'
alias migrate='cd $UPTIMUS && rake db:migrate && rake db:test:clone'
alias p='psql uptimus_development'
alias sass='sass-convert --from css --to sass -R'
alias sidekiq='cd $UPTIMUS && bundle exec sidekiq --config ./config/sidekiq.yml'

# rails

alias r='rails'
alias rc='cd $UPTIMUS && rails console'
alias rd='cd $UPTIMUS && rails dbconsole'
alias rg='cd $UPTIMUS && rails generate'
alias rs='cd $UPTIMUS && rails server'

# ssh

alias staging="ssh devops@94.77.64.80"

# zeus

#alias z='zeus'
#alias zc='cd $UPTIMUS && zeus console'
#alias zd='cd $UPTIMUS && zeus dbconsole'
#alias zg='cd $UPTIMUS && zeus generate'
#alias zr='cd $UPTIMUS && zeus rake'
#alias zs='cd $UPTIMUS && zeus server'

#-----------------------------------------------------------------------------------------
# common
#-----------------------------------------------------------------------------------------

alias cdd='cd ~/Downloads'
alias cdr='cd $REACTOR'
alias cdu='cd $UPTIMUS'
alias df='df -h'
alias ll='ls -alp'
alias m='open -a MacVim'
