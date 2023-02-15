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

    # https://stackoverflow.com/a/7261049
    local last_tag=$(git describe --tags --abbrev=0 2> /dev/null)

    if [[ ! -z $last_tag ]]; then
      echo -n %F{075}
      echo -n " $last_tag"
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

kcr() {
  local POD=
  local HOST=
  local DATABASE=

  if [[ "$1" == "dpn" ]]; then
    POD="dev-platform-namespace-db-client"
    HOST="dev-platform-namespace-db-public"
    DATABASE="dpn"
  fi

  if [[ "$1" == "dpc" ]]; then
    POD="dev-platform-catalog-db-client"
    HOST="dev-platform-catalog-db-public"
    DATABASE="dpc"
  fi

  if [[ -z $NAME ]]; then
    echo "Unknown service: '$1'"
    return 1
  fi

  kubectl exec -it $POD -n platform -- cockroach sql --certs-dir=/cockroach/cockroach-certs --host=$HOST --database=$DATABASE
}

kl() {
  local NAME=

  if [[ "$1" == "dpn" ]]; then
    NAME="dev-platform-namespace"
  fi

  if [[ "$1" == "dpc" ]]; then
    NAME="dev-platform-catalog"
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
  # Kill all kubectl processes on Ctrl-C
  trap "pkill kubectl" SIGINT

  kubectl port-forward service/access-service-db-public -n platform 26260:26257 &
  kubectl port-forward service/catalog-db-public -n platform 26261:26257 &
  kubectl port-forward service/namespace-db-public -n platform 26262:26257 &
  kubectl port-forward service/user-service-db-public -n platform 26263:26257 &

  # Run function in foreground
  wait
}
