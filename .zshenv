# don't use /etc/zprofile - it might reorder directories in PATH:
# https://mattprice.me/2015/zsh-path-issues-osx-el-capitan/
setopt no_global_rcs

#-------------------------------------------------------------------------------
#
# environment variables
#
# export variable if you want programs run from zsh to see it
#
#-------------------------------------------------------------------------------

# removes duplicate entries from PATH
typeset -U path

# NOTE: /opt/chefdk/bin/ is added to PATH in ~/.zlogin
# NOTE: /usr/local/bin/ contains different symlinks added by brew
path=(~/scripts /usr/local/bin $path)
# add bin/ directory of old version of postgresql instead of
# creating symlinks for each binary manually
path=(/usr/local/Cellar/postgresql@9.5/9.5.10/bin $path)

# for iTerm2 to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8

export EDITOR=mvim
export HOMEBREW_GITHUB_API_TOKEN=03adebc410e1f8de5a2765a5f5890ff8beb76d5f

# for gpg to sign commits
# https://github.com/keybase/keybase-issues/issues/1712#issuecomment-141226705
export GPG_TTY=$(tty)

CDPATH=~:~/dev
TERM='xterm-256color'

HISTSIZE=100000
HISTFILESIZE=200000

setopt HIST_IGNORE_DUPS
# don't save command in history if it starts with space
setopt HIST_IGNORE_SPACE

# Elixir

# https://hexdocs.pm/iex/IEx.html#module-shell-history
export ERL_AFLAGS='-kernel shell_history enabled'

# Android

# required by `react-native-cli` to be set
export ANDROID_HOME=/usr/local/share/android-sdk
# might be required by `emulator` to be set
#export ANDROID_SDK_ROOT=/usr/local/share/android-sdk
# `android`
path=($path $ANDROID_HOME/tools)
# `sdkmanager` and `avdmanager`
path=($path $ANDROID_HOME/tools/bin)
# `adb`
path=($path $ANDROID_HOME/platform-tools)

# iOS

# https://github.com/bradmartin/nativescript-videoplayer/issues/76
export SIMCTL_CHILD_OS_ACTIVITY_MODE='disable'

#-------------------------------------------------------------------------------
#
# aliases
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# cd
#-------------------------------------------------------------------------------

alias blog='cd ~/blog'
alias dot='cd ~/.dotfiles'
alias dl='cd ~/Downloads'

alias ic='cd ~/dev/complead/iceperk'
alias ica='cd ~/dev/complead/iceperkapp'
alias icc='cd ~/dev/complead/iceperkchef'
alias icb='cd ~/dev/complead/iceperkbilling'
alias icbc='cd ~/dev/complead/iceperkbillingchef'
alias n='cd ~/dev/morr/neko-achievements'
alias p='cd ~/dev/ingate/pumba'
alias rb='cd ~/dev/ingate/reenter_builder'
alias rf='cd ~/dev/ingate/reenter_feed'
alias s='cd ~/dev/morr/shikimori'

alias opr7='cd /Volumes/opr7'
alias pi='cd /Volumes/pi'

#-------------------------------------------------------------------------------
# system
#-------------------------------------------------------------------------------

alias df='df -h'
alias ll='ls -alp'
alias m='mvim'
alias mail='less +G /var/mail/tap'
alias mcu='mc -u'

#-------------------------------------------------------------------------------
# dev
#
# alias definitions are recursive, the order in which they are defined doesn't
# matter - they can still use each other (so called hoisting in JavaScript)
#-------------------------------------------------------------------------------

# elixir

alias iex='iex -S mix'

# git

alias g='git'
alias ga='git add -A .'
alias gc='noglob git_commit'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git_log'
alias gp='git push'
alias gs='git status'
alias gbd='git_branch_delete'

# rails

alias log='tail -f log/development.log'

alias r='rails'
alias rc='rails console'
alias rd='rails dbconsole'
alias rs='rails server'

alias create_user='rake db:create_user && RAILS_ENV=test rake db:create_user'
alias create='rake db:create && RAILS_ENV=test rake db:create'
alias schema_load='rake db:schema:load && RAILS_ENV=test rake db:schema:load'
alias migrate='rake db:migrate && rake db:test:clone'
alias rollback='rake db:rollback && RAILS_ENV=test rake db:rollback'
alias drop='rake db:drop && RAILS_ENV=test rake db:drop'
alias recreate='rake db:recreate && RAILS_ENV=test rake db:recreate'

# react native

alias and='react-native run-android'
alias ios='react-native run-ios'
alias ios4="react-native run-ios --simulator 'iPhone 4s'"
alias ios5="react-native run-ios --simulator 'iPhone 5'"
alias logios='react-native log-ios | ccze -A -o nolookups'
alias logand='react-native log-android | ccze -A -o nolookups'

# emulator points to /usr/local/bin/emulator
alias avd='$ANDROID_HOME/emulator/emulator -avd Nexus_5X_API_23_x86_64 -gpu host -skin 1080x1920'

# ruby, gems

alias cap='bundle exec cap'
alias guard='bundle exec guard'
alias sidekiq='bundle exec sidekiq --config ./config/sidekiq.yml'

# shikimori

alias git-sub-up="git submodule foreach 'git fetch origin --tags; git checkout master; git pull' && git pull && git submodule update --init --recursive"
alias shikisync=sync_shikimori_images

# SSH
#
# it's to possible to SSH in 2 ways:
#
# 1) ssh <host from .ssh/config>
# 2) ssh <username>@<IP or domain>
#
# in the 1st case user is supplied from .ssh/config
#
# in both cases public key (default one or the one specified in SSH config
# record) is used to authenticate user if it has been previously added to
# .ssh/authorized_keys on server
#
# in case user is not authorized (supplied public key is not present in
# .ssh/authorized_keys on server) SSH fallbacks to password-based authentication

#-------------------------------------------------------------------------------
#
# asdf initialization
#
# it seems completions are not available for zsh yet:
# https://github.com/asdf-vm/asdf/tree/master/completions
#
#-------------------------------------------------------------------------------

source $HOME/.asdf/asdf.sh

#-------------------------------------------------------------------------------
#
# rbenv initialization (invoke after setting PATH)
#
#-------------------------------------------------------------------------------

eval "$(rbenv init -)"

#-------------------------------------------------------------------------------
#
# functions
#
#-------------------------------------------------------------------------------

f() {
  find . -type f \
    \( -name "*.rb" -or -name "*.erb" -or -name "*.rss" -or -name "*.xml" -or -name "*.slim" -or -name "*.haml" -or \
       -name "*.html" -or -name "*.js" -or -name "*.coffee" -or -name "*.ejs" -or -name "*.jst" -or -name "*.eco" -or \
       -name "*.css" -or -name "*.scss" -or -name "*.sass" -or -name "*.yml" -or -name "*.vim" -or -name "*.rabl" -or \
       -name "*.builder"  -or -name "*.txt" \) \
    -exec grep -l "$*" {} \;
}

fvim() {
  mvim `f "$*"`
}

gr() {
  fgrep --color --exclude-dir={log,public,tmp,.git} -Iir "$@" .
}

hitch() {
  command hitch "$@"
  if [[ -s "$HOME/.hitch_export_authors" ]]; then source "$HOME/.hitch_export_authors" ; fi
}

# blog

post() {
  noglob rake "post:create[$*]"
}

publish() {
  git add -A .
  git commit -S -m "update `date +%Y-%m-%d_%H:%M:%S`"
  git push
}

# chef

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

# git

# https://unix.stackexchange.com/questions/274257
git_commit() {
  [[ -z $1 ]] && echo error: specify git commit message && return 1
  git commit -S -m "$*"
}

# current branch is prefixed with `*` in `git branch` output
git_branch_delete() {
  git branch | grep -v -E '(master|develop|\*)' | xargs git branch -d
}

git_log() {
  local format='%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr'
  git log --graph --pretty=format:${format} --abbrev-commit
}

# shikimori

sync_shikimori_images() {
  local local_path=~/shikimori.org/images/
  local shiki_path=/home/apps/shikimori/production/shared/public/images/

  for dir in $(ssh devops@78.46.50.20 ls $shiki_path)
  do
    if [[ "$dir" == "image" || "$dir" == "user_image" || "$dir" == "screenshot" || "$dir" == "cosplay_image" || "$dir" == "webm_video" ]]; then
      echo "skipping $dir"
      continue
    else
      echo "processing $dir ..."
      rsync -urv -e ssh devops@78.46.50.20:$shiki_path$dir $local_path
    fi
  done
}
