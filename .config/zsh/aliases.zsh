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
alias dpi='cd ~/dev/indrive/dev-platform-infrastructure'
alias dpa='cd ~/dev/indrive/dev-platform-access-service'

#-------------------------------------------------------------------------------
# System
#-------------------------------------------------------------------------------

alias df='df -h'
# https://stackoverflow.com/a/30677813/3632318
alias e='open -a Emacs --args --chdir $PWD "$@"'
alias ll='ls -alp'
alias m='mvim'
alias mcu='mc -u'
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

alias cr='cockroach start-single-node --insecure --http-port=26256 --host=localhost'
alias d='docker'
alias k='kubectl'
alias kcr='kubectl exec -it dev-platform-orchestrator-db-client -n platform -- cockroach sql --certs-dir=/cockroach/cockroach-certs --host=dev-platform-orchestrator-db-public --database=dpo'
alias kl='kubectl logs -fl app.kubernetes.io/name=dev-platform-orchestrator -n platform | jq "{timestamp,level,message}"'
alias kpf='kubectl port-forward service/dev-platform-orchestrator-db-public -n platform 26257'

#-------------------------------------------------------------------------------
# Clojure
#-------------------------------------------------------------------------------

alias rc='clj -M:dev:repl'

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
