#===============================================================================
# environment variables
#===============================================================================

#-------------------------------------------------------------------------------
# system
#-------------------------------------------------------------------------------

set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR mvim
set -x PAGER less

#-------------------------------------------------------------------------------
# Elixir
#-------------------------------------------------------------------------------

# https://hexdocs.pm/iex/IEx.html#module-shell-history
set -x ERL_AFLAGS '-kernel shell_history enabled'

#-------------------------------------------------------------------------------
# React Native
#-------------------------------------------------------------------------------

# set React Native Debugger as JavaScript debugger
# https://github.com/jhen0409/react-native-debugger/blob/master/docs/getting-started.md#launch-by-cli-or-react-native-packager-macos-only
# https://github.com/artsy/emission/blob/45417ca425f2cba7d2da21902ef8ff1cd093a024/package.json#L28
set -x REACT_DEBUGGER "open -g 'rndebugger://set-debugger-loc?port=8081' --args"

#-------------------------------------------------------------------------------
# iOS
#-------------------------------------------------------------------------------

# https://github.com/bradmartin/nativescript-videoplayer/issues/76
set -x SIMCTL_CHILD_OS_ACTIVITY_MODE 'disable'

#-------------------------------------------------------------------------------
# Android
#-------------------------------------------------------------------------------

# required by `react-native-cli` to be set
set -x ANDROID_HOME /usr/local/share/android-sdk
# `android`
set -x PATH $PATH $ANDROID_HOME/tools
# `sdkmanager` and `avdmanager`
set -x PATH $PATH $ANDROID_HOME/tools/bin
# `adb`
set -x PATH $PATH $ANDROID_HOME/platform-tools

#===============================================================================
# aliases
#===============================================================================

alias f='functions'

#-------------------------------------------------------------------------------
# cd
#-------------------------------------------------------------------------------

alias blog='cd ~/blog'
alias dot='cd ~/.dotfiles'

alias a='cd ~/dev/compleader/ansible'
alias alice='cd ~/dev/compleader/alice'
alias christine='cd ~/dev/compleader/alice/apps/christine'
alias francesca='cd ~/dev/compleader/alice/apps/francesca'
alias ic='cd ~/dev/compleader/iceperk'
alias ica='cd ~/dev/compleader/iceperkapp'
alias icb='cd ~/dev/compleader/iceperkbilling'
alias icbc='cd ~/dev/compleader/iceperkbillingchef'
alias icc='cd ~/dev/compleader/iceperkchef'
alias lain='cd ~/dev/compleader/lain'
alias lucy='cd ~/dev/compleader/lucy'
alias martha='cd ~/dev/compleader/alice/apps/martha'
alias neko='cd ~/dev/morr/neko-achievements'
alias reika='cd ~/dev/compleader/reika'
alias sith='cd ~/dev/compleader/sith'
alias sithchef='cd ~/dev/compleader/sithchef'
alias veronika='cd ~/dev/compleader/alice/apps/veronika'
alias yancy='cd ~/dev/compleader/alice/apps/yancy'

#-------------------------------------------------------------------------------
# system
#-------------------------------------------------------------------------------

alias df='df -h'
alias ll='ls -alp'
alias m='mvim'
alias mcu='mc -u'
# http://reasoniamhere.com/2014/01/11/outrageously-useful-tips-to-master-your-z-shell/
# (#i) - case-insensitive globbing
# (Om) - sort by modification date (asc)
alias q='open -Fn (#i)*.(jpeg|jpg|png)(Om)'

#-------------------------------------------------------------------------------
# dev (general)
#-------------------------------------------------------------------------------

alias h='honcho start'

#-------------------------------------------------------------------------------
# Ansible
#-------------------------------------------------------------------------------

alias ag='ansible-galaxy'
alias ap='ansible-playbook'
alias av='ansible-vault'

#-------------------------------------------------------------------------------
# Elixir
#-------------------------------------------------------------------------------

alias iex='iex -S mix'

#-------------------------------------------------------------------------------
# Git
#-------------------------------------------------------------------------------

alias g='git'
alias ga='git add -A .'
alias gbd='git_branch_delete'
alias gc='git_commit'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git_log'
alias gp='git push'
alias gpf='git push --force'
alias gs='git status'

#-------------------------------------------------------------------------------
# Rails
#-------------------------------------------------------------------------------

alias r='rails'
alias rc='rails console'
alias rs='rails server'

alias cap='bundle exec cap'
alias guard='bundle exec guard'
alias sidekiq='bundle exec sidekiq --config ./config/sidekiq.yml'

#-------------------------------------------------------------------------------
# React Native
#-------------------------------------------------------------------------------

alias rn='react-native'

alias npm_reset='\
  watchman watch-del-all &&
  rm -rf "$TMPDIR/react-native-packager-cache-*" &&
  rm -rf "$TMPDIR/metro-bundler-cache-*" &&
  rm -rf "$TMPDIR/haste-map-react-native-packager-*" &&
  rm -rf node_modules &&
  npm cache clean --force &&
  npm install
  '

alias yarn_reset='\
  watchman watch-del-all &&
  rm -rf "$TMPDIR/react-native-packager-cache-*" &&
  rm -rf "$TMPDIR/metro-bundler-cache-*" &&
  rm -rf "$TMPDIR/haste-map-react-native-packager-*" &&
  rm -rf node_modules &&
  yarn cache clean &&
  yarn install
  '

alias ios='react-native run-ios'
alias ios4="react-native run-ios --simulator 'iPhone 4s'"
alias ios5="react-native run-ios --simulator 'iPhone 5'"
alias ios6p="react-native run-ios --simulator 'iPhone 6 Plus'"
alias logios='react-native log-ios | ccze -A -o nolookups'

alias avd='$ANDROID_HOME/tools/emulator -avd Nexus_5X_API_23_x86_64 -gpu host -skin 1080x1920'
alias android='react-native run-android'
alias logand='react-native log-android | ccze -A -o nolookups'
alias build_android_release='cd android && ./gradlew assembleRelease; cd ..'

#===============================================================================
# initialization
#===============================================================================

# asdf is used for both Elixir and Ruby
source $HOME/.asdf/asdf.fish

#===============================================================================
# functions
#===============================================================================

#-------------------------------------------------------------------------------
# blog
#-------------------------------------------------------------------------------

function post
  rake post:create[$argv]
end

function publish
  git add -A .
  git commit -S -m 'update '(date +%Y-%m-%d_%H:%M:%S)
  git push
end

#-------------------------------------------------------------------------------
# Git
#-------------------------------------------------------------------------------

# https://unix.stackexchange.com/questions/274257
function git_commit
  if test -z $argv[1]
    echo 'error: specify git commit message'
    return 1
  end

  git commit -S -m "$argv"
end

# current branch is prefixed with '*' in 'git branch' output
function git_branch_delete
  git branch | grep -v -E '(master|develop|\*)' | xargs git branch -d
end

function git_log
  set -l format '%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr'
  git log --graph --pretty=format:$format --abbrev-commit
end

#===============================================================================
# colors
#
# http://fishshell.com/docs/current/#variables-color
# http://fishshell.com/docs/current/commands.html#set_color
#===============================================================================

set fish_color_command 27AD2F
set fish_color_error FF93B2
set fish_color_param normal

#===============================================================================
# MacVim defaults
#
# https://github.com/macvim-dev/macvim/wiki/FAQ
# :h macvim-prefs
#
# NOTE: it's not required to write defaults every time new shell is
#       started - they are persisted across sessions
#
# in general preferences are set by specifying their type and value:
# - `default write org.vim.MacVim MMShowAddTabButton -bool false`
#
# in most cases, I guess, type can be omitted:
# - `default write org.vim.MacVim MMShowAddTabButton 0`
# - `default write org.vim.MacVim MMShowAddTabButton false`
#
# reset current preferences:
# $ defaults delete org.vim.MacVim
#
# show current preferences:
# $ defaults read org.vim.MacVim
#===============================================================================

#defaults write org.vim.MacVim MMTextInsetTop 1
#defaults write org.vim.MacVim MMTextInsetRight 3
#defaults write org.vim.MacVim MMTextInsetBottom 5
#defaults write org.vim.MacVim MMTextInsetLeft 5
# https://github.com/macvim-dev/macvim/issues/390#issuecomment-254252969
#defaults write org.vim.MacVim SUEnableAutomaticChecks 0
