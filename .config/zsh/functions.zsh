#*******************************************************************************
#
# Functions
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# Blog
#-------------------------------------------------------------------------------

post() {
  noglob rake "post:create[$*]"
}

publish() {
  git add -A .
  git commit -S -m "update $(date +%Y-%m-%d_%H:%M:%S)"
  git push
}

#-------------------------------------------------------------------------------
# Git
#-------------------------------------------------------------------------------

# https://joshtronic.com/2018/01/28/minimalist-git-prompt/
# https://joshdick.net/2017/06/08/my_git_prompt_for_zsh_revisited.html
# https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Zsh
git_prompt() {
  # https://stackoverflow.com/a/11868440/3632318
  local branch=$(git symbolic-ref --short HEAD 2> /dev/null)

  # https://wiki.archlinux.org/index.php/Zsh#Colors
  if [[ ! -z $branch ]]; then
    echo -n %F{075}
    echo -n '('
    echo -n $branch

    if [[ ! -z "$(git status --short)" ]]; then
      echo -n %F{214}
      echo -n '*'
      echo -n %F{reset}
    fi

    echo -n %F{075}
    echo -n ') '
    echo -n %F{reset}
  fi
}

# https://unix.stackexchange.com/questions/274257
git_commit() {
  if [[ -z $1 ]]; then
    echo 'error: specify git commit message'
    return 1
  fi

  git commit -S -m "$*"
}

# Current branch is prefixed with '*' in 'git branch' output
git_branch_delete() {
  git branch | grep -v -E '(master|develop|\*)' | xargs git branch -d
}

git_log() {
  local format='%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr'
  git log --graph --pretty=format:${format} --abbrev-commit
}

kl() {
  local NAME=

  if [[ "$1" == "dpo" ]]; then
    NAME="dev-platform-orchestrator"
  fi

  if [[ "$1" == "dpas" ]]; then
    NAME="dev-platform-access-service"
  fi

  if [[ "$1" == "dpus" ]]; then
    NAME="dev-platform-user-service"
  fi

  if [[ -z $NAME ]]; then
    echo "Unknown service: '$1'"
    return 1
  fi

  # https://github.com/kubernetes/kubectl/issues/917
  # https://stackoverflow.com/a/58649439/3632318
  kubectl logs -fl "app.kubernetes.io/name=$NAME" -n platform --tail -1 | jq -r '[.timestamp, .level, .message]|@tsv' -C
}

kpf() {
  kubectl port-forward service/dev-platform-access-service-db-public -n platform 26260:26257 &
  kubectl port-forward service/dev-platform-orchestrator-db-public -n platform 26261:26257 &
  kubectl port-forward service/dev-platform-user-service-db-public -n platform 26262:26257 &
}
