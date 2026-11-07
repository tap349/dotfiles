#-------------------------------------------------------------------------------
# System
#-------------------------------------------------------------------------------

alias dl='cd ~/Downloads'
alias dot='cd ~/.dotfiles'
alias org='cd ~/org-roam'

alias df='df -h'
alias ll='ls -alph'

# Midnight Commander doesn't fully recognize xterm-ghostty terminal type
# => treat Ghostty as a generic xterm-compatible terminal
alias mcu='TERM=xterm-256color mc -U'

# http://reasoniamhere.com/2014/01/11/outrageously-useful-tips-to-master-your-z-shell/
#
# (#i) - case-insensitive globbing
# (Om) - sort by modification date (asc)
alias q='open -Fn (#i)*.(jpeg|jpg|png|webp)(Om)'

#-------------------------------------------------------------------------------
# Dev
#-------------------------------------------------------------------------------

alias d='docker'
alias dc='docker compose'
# https://stackoverflow.com/a/30677813/3632318
alias e='open -a Emacs --args --chdir $PWD "$@"'
alias k='kubectl'
alias kctx='kubectx'
alias kk='k9s'
alias m='mvim'

#-------------------------------------------------------------------------------
# git
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

git_branch_delete() {
  # Current branch is prefixed with '*' in `git branch` output
  git branch | grep -v -E '(main|develop|\*)' | xargs git branch -d
}

git_commit() {
  if [[ -z $1 ]]; then
    echo 'Error: specify git commit message'
    return 1
  fi

  git commit -S -m "$*"
}

git_log() {
  local format='%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr'
  git log --graph --pretty=format:${format} --abbrev-commit
}
