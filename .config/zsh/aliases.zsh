#*******************************************************************************
#
# Aliases
#
# Alias definitions are recursive, the order in which they are defined doesn't
# matter - they can still use each other (so called hoisting in JavaScript)
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# cd
#-------------------------------------------------------------------------------

alias blog='cd ~/blog'
alias dot='cd ~/.dotfiles'

alias ka='cd ~/dev/indrive/k8s-applications'
alias dpo='cd ~/dev/indrive/dev-platform-orchestrator'

#-------------------------------------------------------------------------------
# System
#-------------------------------------------------------------------------------

alias df='df -h'
# https://stackoverflow.com/a/30677813/3632318
alias e='open -a Emacs --args --chdir $PWD "$@"'
alias ll='ls -alp'
alias m='mvim'
alias mcu='mc -u'
# http://reasoniamhere.com/2014/01/11/outrageously-useful-tips-to-master-your-z-shell/
#
# (#i) - case-insensitive globbing
# (Om) - sort by modification date (asc)
alias q='open -Fn (#i)*.(jpeg|jpg|png)(Om)'
alias ydl='youtube-dl'

#-------------------------------------------------------------------------------
# Dev
#-------------------------------------------------------------------------------

alias o='overmind start'

#-------------------------------------------------------------------------------
# Clojure
#-------------------------------------------------------------------------------

alias rc='clj -M:repl'

#-------------------------------------------------------------------------------
# Git
#-------------------------------------------------------------------------------

alias g='git'
alias ga='git add -A .'
alias gbd='git_branch_delete'
alias gc='noglob git_commit'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git_log'
alias gp='git push'
alias gs='git status'

#-------------------------------------------------------------------------------
# Kubernetes
#-------------------------------------------------------------------------------

alias k='kubectl'
