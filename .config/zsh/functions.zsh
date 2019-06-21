#*******************************************************************************
#
# functions
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# blog
#-------------------------------------------------------------------------------

post() {
  noglob rake "post:create[$*]"
}

publish() {
  git add -A .
  git commit -S -m "update `date +%Y-%m-%d_%H:%M:%S`"
  git push
}

#-------------------------------------------------------------------------------
# Chef
#-------------------------------------------------------------------------------

# https://stackoverflow.com/a/5955623/3632318
# https://stackoverflow.com/a/34533957/3632318
converge() {
  local name="$1"
  local env="$2"

  if [ -z $env ]; then env='prod'; fi

  # it's much faster than `chef exec knife node environment_set $name $env`
  sed -i '' -e "/chef_environment/ s/: \".*\"/: \"${env}\"/" nodes/${name}.json
  chef exec berks vendor && chef exec knife zero converge "name:${name}"
}

#-------------------------------------------------------------------------------
# Git
#-------------------------------------------------------------------------------

# https://unix.stackexchange.com/questions/274257
git_commit() {
  if [[ -z $1 ]]; then
    echo 'error: specify git commit message'
    return 1
  fi

  git commit -S -m "$*"
}

# current branch is prefixed with '*' in 'git branch' output
git_branch_delete() {
  git branch | grep -v -E '(master|develop|\*)' | xargs git branch -d
}

git_log() {
  local format='%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr'
  git log --graph --pretty=format:${format} --abbrev-commit
}
