# don't use /etc/zprofile - it might reorder directories in PATH
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

# NOTE: add /opt/chefdk/bin to PATH in ~/.zlogin
path=(~/scripts /usr/local/bin $path)

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

# for iterm to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8

export EDITOR=mvim
export HOMEBREW_GITHUB_API_TOKEN=03adebc410e1f8de5a2765a5f5890ff8beb76d5f

CDPATH=~:~/dev
TERM='xterm-256color'

HISTSIZE=100000
HISTFILESIZE=200000
setopt HIST_IGNORE_DUPS

BLOG=~/blog
DOTFILES=~/.dotfiles
DOWNLOADS=~/Downloads

# projects

ICEPERK=~/dev/complead/iceperk
ICEPERKAPP=~/dev/complead/iceperkapp
NEKO_ACHIEVEMENTS=~/dev/morr/neko-achievements
NTV_CLIENT=~/dev/ntv/client
NTV_SYSTEMD_UNITS=~/dev/ntv/systemd_units
NTV_WIFI=~/dev/ntv/wifi
PUMBA=~/dev/ingate/pumba
REENTER_BUILDER=~/dev/ingate/reenter_builder
REENTER_FEED=~/dev/ingate/reenter_feed
SHIKIMORI=~/dev/morr/shikimori

# mount points

OPR7=/Volumes/opr7
PI=/Volumes/pi

#-------------------------------------------------------------------------------
#
# aliases
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# cd
#-------------------------------------------------------------------------------

alias blog='cd $BLOG'
alias dot='cd $DOTFILES'
alias dl='cd $DOWNLOADS'

alias rb='cd $REENTER_BUILDER'
alias rf='cd $REENTER_FEED'
alias ic='cd $ICEPERK'
alias ica='cd $ICEPERKAPP'
alias n='cd $NEKO_ACHIEVEMENTS'
alias nc='cd $NTV_CLIENT'
alias nu='cd $NTV_SYSTEMD_UNITS'
alias nw='cd $NTV_WIFI'
alias p='cd $PUMBA'
alias s='cd $SHIKIMORI'

alias opr7='cd $OPR7'
alias pi='cd $PI'

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
#-------------------------------------------------------------------------------

# android

alias emulator='cd /usr/local/share/android-sdk/emulator && ./emulator'

# elixir

alias iex='iex -S mix'

# rails, rake, rspec

alias rails='bin/rails'
alias rake='bin/rake'
alias rspec='bin/rspec'

alias r='bin/rails'
alias rc='bin/rails console'
alias rd='bin/rails dbconsole'
alias rs='bin/rails server'

alias create_user='bin/rake db:create_user && RAILS_ENV=test bin/rake db:create_user'
alias create='bin/rake db:create && RAILS_ENV=test bin/rake db:create'
alias schema_load='bin/rake db:schema:load && RAILS_ENV=test bin/rake db:schema:load'
alias migrate='bin/rake db:migrate && RAILS_ENV=test bin/rake db:migrate'
alias rollback='bin/rake db:rollback && RAILS_ENV=test bin/rake db:rollback'
alias drop='bin/rake db:drop && RAILS_ENV=test bin/rake db:drop'
alias recreate='bin/rake db:recreate && RAILS_ENV=test bin/rake db:recreate'

# other gems

alias cap='bundle exec cap'
alias guard='bundle exec guard'
alias sidekiq='bundle exec sidekiq --config ./config/sidekiq.yml'

# git

alias g='git'
alias ga='git add -A .'
alias gc='git_commit'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl="git log --graph --pretty=format:'%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr' --abbrev-commit"
alias gs='git status'
alias gbd='git_delete_branches'

# misc

alias css_to_sass='sass-convert --from css --to sass -R'
alias log='tail -f log/development.log'

# shikimori

alias git-sub-up="git submodule foreach 'git fetch origin --tags; git checkout master; git pull' && git pull && git submodule update --init --recursive"
alias shikisync=sync_shikimori_images

# SSH
#
# it's to possible to SSH in 2 ways:
#
# 1) ssh <host from .ssh/config>
# 2) ssh <username>@<host from /etc/hosts>
#
# in the 1st case user is supplied from .ssh/config
#
# in both cases public key for matching host is used to authenticate user
# if it has been previously added to .ssh/authorized_keys on server
#
# in case matching host can't be found in .ssh/config ssh fallbacks
# to password-based authentication (e.g. when using IP or alternative domain)

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
  git commit -m "update `date +%Y-%m-%d_%H:%M:%S`"
  git push
}

# git

git_commit() {
  [[ -z $1 ]] && echo error: specify git commit message && return 1
  git commit -m "$*"
}

git_delete_branches() {
  git branch | grep -v -E '(master|develop)' | xargs git branch -d
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
