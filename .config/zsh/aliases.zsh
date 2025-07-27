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

alias dot='cd ~/.dotfiles'

#-------------------------------------------------------------------------------
# System
#-------------------------------------------------------------------------------

# alias colima='colima start -m 5 -d 100'
alias df='df -h'
alias ll='ls -alph'
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

alias d='docker'
alias dc='docker compose'
# https://stackoverflow.com/a/30677813/3632318
alias e='open -a Emacs --args --chdir $PWD "$@"'
alias k='kubectl'
alias kctx='kubectx'
alias ks='k9s'
alias m='mvim'

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
alias gpt='git push --tags'
alias gs='git status'

#-------------------------------------------------------------------------------
# Misc
#-------------------------------------------------------------------------------

alias pali='open ~/docs/Pali/ГРАМ\ (0[2-9]|1[0-9])\ *'

#-------------------------------------------------------------------------------
# inDrive
#-------------------------------------------------------------------------------

alias dpa='cd ~/dev/indrive/dev-platform-access'
alias dpal='cd ~/dev/indrive/dev-platform-activity-log'
alias dpb='cd ~/dev/indrive/dev-platform-backup'
alias dpd='cd ~/dev/indrive/dev-platform-deployment'
alias dpdd='cd ~/dev/indrive/dev-platform-deployer'
alias dpeg='cd ~/dev/indrive/dev-platform-external-gateway'
alias dpg='cd ~/dev/indrive/dev-platform-gateway'
alias dpgc='cd ~/dev/indrive/dev-platform-go-common'
alias dpi='cd ~/dev/indrive/dev-platform-installer'
alias dpis='cd ~/dev/indrive/dev-platform-infrastructure'
alias dpmr='cd ~/dev/indrive/dev-platform-mysql-runner'
alias dpn='cd ~/dev/indrive/dev-platform-namespace'
alias dpnt='cd ~/dev/indrive/dev-platform-notifier'
alias dpqg='cd ~/dev/indrive/dev-platform-quality-gates'
alias dpr='cd ~/dev/indrive/dev-platform-runner'
alias dpsh='cd ~/dev/indrive/dev-platform-service-hub'
alias dpui='cd ~/dev/indrive/dev-platform-ui'

alias sso='aws sso login --profile devplatform_team-531211996670 && aws sso login --profile inDriveAdministratorAccess-627723547655'

alias tp='telepresence'
# alias tpc='tp connect -n platform --mapped-namespaces platform --allow-conflicting-subnets 10.0.0.0/8'
alias tpc='tp connect -n platform --mapped-namespaces platform --vnat all'
