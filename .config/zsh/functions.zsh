#*******************************************************************************
#
# Functions
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# System
#-------------------------------------------------------------------------------

# See also https://github.com/occivink/mpv-image-viewer
mvi() {
  mpv --force-window=immediate --image-display-duration=inf "$1"
  osascript -e 'tell application "iTerm" to activate'
}

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

# Current branch is prefixed with '*' in `git branch` output
git_branch_delete() {
  git branch | grep -v -E '(main|master|develop|\*)' | xargs git branch -d
}

# https://unix.stackexchange.com/questions/274257
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

#-------------------------------------------------------------------------------
# kubectl
#-------------------------------------------------------------------------------

# $1 - namespace-db
# $2 - dpn
kcr() {
  local POD="$1-client"
  local HOST="$1-public"
  local DATABASE="$2"

  kubectl exec -it $POD -n platform -- \
    cockroach sql --certs-dir=/cockroach/cockroach-certs --host=$HOST --database=$DATABASE
}

# $1 - dev-platform-namespace
#
# https://github.com/kubernetes/kubectl/issues/917
# https://stackoverflow.com/a/58649439/3632318
kl() {
  # https://jamesdefabia.github.io/docs/user-guide/kubectl/kubectl_logs/
  # Golang: level, ts, msg, status
  #
  # Convert level to upper case for correct highlighting by iTerm rules
  kubectl logs -fl "app.kubernetes.io/name=$1" -n platform --tail -1 --since 30m \
    | jq -r '[.ts, (.level | ascii_upcase), .msg, .status]|@tsv' -C
}

# $1 - dev-platform-namespace
ksh() {
  kubectl exec -it "deployment/$1" -n platform -- /bin/bash
}

kpf() {
  # Kill all kubectl processes on Ctrl-C
  trap "pkill kubectl" SIGINT

  kubectl port-forward service/catalog-db-public -n platform 26261:26257 &
  kubectl port-forward service/namespace-db-public -n platform 26262:26257 &
  kubectl port-forward service/user-service-db-public -n platform 26263:26257 &
  kubectl port-forward service/pipeline-db-public -n platform 26264:26257 &
  kubectl port-forward service/template-db-public -n platform 26265:26257 &
  kubectl port-forward service/backup-db-public -n platform 26266:26257 &

  # Run function in foreground
  wait
}
