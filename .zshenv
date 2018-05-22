# don't use /etc/zprofile - it might reorder directories in PATH:
# https://mattprice.me/2015/zsh-path-issues-osx-el-capitan/
setopt no_global_rcs
# say, to be able to use case-insensitive globbing (#i)
setopt extendedglob

#===============================================================================
#
# environment variables
#
# export variable if you want programs run from Zsh to see it
#
#===============================================================================

# removes duplicate entries from PATH
typeset -U path

# NOTE: /usr/local/bin/ contains different symlinks added by brew
path=(~/scripts /usr/local/bin $path)
# add bin/ directory of old version of postgresql instead of
# creating symlinks for each binary manually
#
# UPDATE: now postgresql@9.5 is run inside docker container so we
# can use bin/ directory of the latest version of postgresql (10)
#path=(/usr/local/Cellar/postgresql@9.5/9.5.10/bin $path)

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

#-------------------------------------------------------------------------------
# Elixir
#-------------------------------------------------------------------------------

# https://hexdocs.pm/iex/IEx.html#module-shell-history
export ERL_AFLAGS='-kernel shell_history enabled'

#-------------------------------------------------------------------------------
# React Native
#-------------------------------------------------------------------------------

# set React Native Debugger as JavaScript debugger
# https://github.com/jhen0409/react-native-debugger/blob/master/docs/getting-started.md#launch-by-cli-or-react-native-packager-macos-only
# https://github.com/artsy/emission/blob/45417ca425f2cba7d2da21902ef8ff1cd093a024/package.json#L28
export REACT_DEBUGGER="open -g 'rndebugger://set-debugger-loc?port=8081' --args"

#-------------------------------------------------------------------------------
# iOS
#-------------------------------------------------------------------------------

# https://github.com/bradmartin/nativescript-videoplayer/issues/76
export SIMCTL_CHILD_OS_ACTIVITY_MODE='disable'

#-------------------------------------------------------------------------------
# Android
#-------------------------------------------------------------------------------

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

#===============================================================================
#
# aliases
#
#===============================================================================

#===============================================================================
# cd
#===============================================================================

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
alias s='cd ~/dev/complead/sith'
alias sx='cd ~/dev/complead/sithex'
alias sc='cd ~/dev/complead/sithchef'
alias sxc='cd ~/dev/complead/sithexchef'

alias opr7='cd /Volumes/opr7'
alias pi='cd /Volumes/pi'

#===============================================================================
# system
#===============================================================================

alias df='df -h'
alias ll='ls -alp'
alias m='mvim'
alias mail='less +G /var/mail/tap'
alias mcu='mc -u'
# http://reasoniamhere.com/2014/01/11/outrageously-useful-tips-to-master-your-z-shell/
# (#i) - case-insensitive globbing
# (Om) - sort by modification date (asc)
alias q='open -F (#i)*.(jpeg|jpg|png)(Om)'

#===============================================================================
# dev
#
# alias definitions are recursive, the order in which they are defined doesn't
# matter - they can still use each other (so called hoisting in JavaScript)
#===============================================================================

alias h='honcho start'

#-------------------------------------------------------------------------------
# Elixir
#-------------------------------------------------------------------------------

alias iex='iex -S mix'

#-------------------------------------------------------------------------------
# Git
#-------------------------------------------------------------------------------

alias g='git'
alias ga='git add -A .'
alias gc='noglob git_commit'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git_log'
alias gp='git push'
alias gs='git status'
alias gbd='git_branch_delete'

#-------------------------------------------------------------------------------
# Rails
#-------------------------------------------------------------------------------

alias log='tail -f log/development.log'

alias r='rails'
alias rc='rails console'
alias rd='rails dbconsole'
alias rs='rails server'

# test database is managed automatically in Rails 5:
#
# $ rails db:create
# Created database 'iceperk_development'
# Created database 'iceperk_test'

#-------------------------------------------------------------------------------
# React Native
#-------------------------------------------------------------------------------

# 1. Clear watchman watches: `watchman watch-del-all`.
# 2. Delete the `node_modules` folder: `rm -rf node_modules && npm install`.
# 3. Reset Metro Bundler cache: `rm -rf $TMPDIR/react-*` or `npm start -- --reset-cache`.
# 4. Remove haste cache: `rm -rf $TMPDIR/haste-map-react-native-packager-*`.
alias yarn_reset='watchman watch-del-all && rm -rf "$TMPDIR/react-*" && rm -rf node_modules/ && yarn cache clean && yarn install'

alias ios='react-native run-ios'
alias ios4="react-native run-ios --simulator 'iPhone 4s'"
alias ios5="react-native run-ios --simulator 'iPhone 5'"
alias ios6p="react-native run-ios --simulator 'iPhone 6 Plus'"
alias logios='react-native log-ios | ccze -A -o nolookups'

# emulator points to /usr/local/bin/emulator
alias avd='$ANDROID_HOME/emulator/emulator -avd Nexus_5X_API_23_x86_64 -gpu host -skin 1080x1920'
alias and='react-native run-android'
alias logand='react-native log-android | ccze -A -o nolookups'
alias build_android_release='cd android && ./gradlew assembleRelease; cd ..'

#-------------------------------------------------------------------------------
# Ruby, gems
#-------------------------------------------------------------------------------

alias cap='bundle exec cap'
alias guard='bundle exec guard'
alias sidekiq='bundle exec sidekiq --config ./config/sidekiq.yml'

#-------------------------------------------------------------------------------
# shikimori
#-------------------------------------------------------------------------------

alias git-sub-up="git submodule foreach 'git fetch origin --tags; git checkout master; git pull' && git pull && git submodule update --init --recursive"
alias shikisync=sync_shikimori_images

#-------------------------------------------------------------------------------
# SSH
#-------------------------------------------------------------------------------

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

#===============================================================================
#
# asdf initialization
#
# it seems completions are not available for zsh yet:
# https://github.com/asdf-vm/asdf/tree/master/completions
#
#===============================================================================

source $HOME/.asdf/asdf.sh

#===============================================================================
#
# rbenv initialization (invoke after setting PATH)
#
#===============================================================================

eval "$(rbenv init -)"

#===============================================================================
#
# functions
#
#===============================================================================

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

#-------------------------------------------------------------------------------
# shikimori
#-------------------------------------------------------------------------------

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
