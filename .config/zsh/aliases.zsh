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

alias dpa='cd ~/dev/indrive/dev-platform-access'
alias dpb='cd ~/dev/indrive/dev-platform-backup'
alias dpc='cd ~/dev/indrive/dev-platform-catalog'
alias dpcc='cd ~/dev/indrive/dev-platform-clojure-common'
alias dpd='cd ~/dev/indrive/dev-platform-deployment'
alias dpg='cd ~/dev/indrive/dev-platform-gateway'
alias dpgc='cd ~/dev/indrive/dev-platform-go-common'
alias dpi='cd ~/dev/indrive/dev-platform-installer'
alias dpis='cd ~/dev/indrive/dev-platform-infrastructure'
alias dpmr='cd ~/dev/indrive/dev-platform-mysql-runner'
alias dpn='cd ~/dev/indrive/dev-platform-namespace'
alias dpnt='cd ~/dev/indrive/dev-platform-notifier'
alias dpqg='cd ~/dev/indrive/dev-platform-quality-gates'
alias dpr='cd ~/dev/indrive/dev-platform-runner'
alias dpus='cd ~/dev/indrive/dev-platform-user-service'

#-------------------------------------------------------------------------------
# System
#-------------------------------------------------------------------------------

alias df='df -h'
alias ll='ls -alp'
alias mcu='mc -u'
# See also https://github.com/occivink/mpv-image-viewer
alias mvi='mpv --keep-open=yes'
alias ncdu='ncdu --color off'
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
alias rc='clj -M:defaults:dev:repl'
alias tp='telepresence'

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

alias pali='open -n ~/docs/Pali/ГРАММАТИКА\ ([3-9]|10)\ *'
