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

alias a='cd ~/dev/compleader/ansible'
alias as='cd ~/dev/compleader/ad_sensor'
alias asa='cd ~/dev/compleader/ad_sensor_app'
alias y='cd ~/dev/compleader/yalper'

#-------------------------------------------------------------------------------
# System
#-------------------------------------------------------------------------------

alias df='df -h'
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
# Dev (general)
#-------------------------------------------------------------------------------

alias o='overmind start'

#-------------------------------------------------------------------------------
# Ansible
#-------------------------------------------------------------------------------

alias ag='ansible-galaxy'
alias ap='ansible-playbook'
alias av='ansible-vault'

#-------------------------------------------------------------------------------
# Elixir
#-------------------------------------------------------------------------------

alias iex='iex -S mix'

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
# K8s
#-------------------------------------------------------------------------------

alias k='kubectl'
alias ks='kubectl -n kube-system'
