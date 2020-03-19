#*******************************************************************************
#
# aliases
#
# alias definitions are recursive, the order in which they are defined doesn't
# matter - they can still use each other (so called hoisting in JavaScript)
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# cd
#-------------------------------------------------------------------------------

alias blog='cd ~/blog'
alias dot='cd ~/.dotfiles'

alias a='cd ~/dev/compleader/ansible'
alias cosette='cd ~/dev/compleader/cosette'
alias diana='cd ~/dev/compleader/diana'
alias ic='cd ~/dev/compleader/iceperk'
alias ica='cd ~/dev/compleader/iceperkapp'
alias icb='cd ~/dev/compleader/iceperkbilling'
alias icbc='cd ~/dev/compleader/iceperkbillingchef'
alias icc='cd ~/dev/compleader/iceperkchef'
alias lain='cd ~/dev/compleader/lain'
alias lucy='cd ~/dev/compleader/lucy'
alias ms='cd ~/dev/blackchestnut/minisklad'
alias msa='cd ~/dev/blackchestnut/minisklad-app'
alias msn='cd ~/dev/blackchestnut/minisklad-node'
alias msnc='cd ~/dev/blackchestnut/minisklad-node-chef'
alias neko='cd ~/dev/morr/neko-achievements'
alias reika='cd ~/dev/compleader/reika'
alias sith='cd ~/dev/compleader/sith'
alias sithchef='cd ~/dev/compleader/sithchef'

# alice
alias alice='cd ~/dev/compleader/alice'
alias christine='cd ~/dev/compleader/alice/apps/christine'
alias francesca='cd ~/dev/compleader/alice/apps/francesca'
alias martha='cd ~/dev/compleader/alice/apps/martha'
alias rebecca='cd ~/dev/compleader/alice/apps/rebecca'
alias veronika='cd ~/dev/compleader/alice/apps/veronika'
alias yancy='cd ~/dev/compleader/alice/apps/yancy'

# anna
alias annachef='cd ~/dev/compleader/annachef'
alias corina='cd ~/dev/compleader/corina'
alias ginger='cd ~/dev/compleader/ginger'

#-------------------------------------------------------------------------------
# system
#-------------------------------------------------------------------------------

alias df='df -h'
alias ll='ls -alp'
alias m='mvim'
alias mcu='mc -u'
# http://reasoniamhere.com/2014/01/11/outrageously-useful-tips-to-master-your-z-shell/
#
# (#i) - case-insensitive globbing
# (Om) - sort by modification date (asc)
alias q='open -Fn (#i)*.(jpeg|jpg|png)(Om)'
alias ydl='youtube-dl'

#-------------------------------------------------------------------------------
# dev (general)
#-------------------------------------------------------------------------------

alias o='overmind start'

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
alias gc='noglob git_commit'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git_log'
alias gp='git push'
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

alias e='expo'
alias rn='react-native'

alias npm_reset='\
  watchman watch-del-all &&
  rm -rf "$TMPDIR/react-native-packager-cache-*" &&
  rm -rf "$TMPDIR/metro-bundler-cache-*" &&
  rm -rf "$TMPDIR/haste-map-react-native-packager-*" &&
  rm -rf "node_modules/*" &&
  npm cache clean --force &&
  npm install
  '

alias yarn_reset='\
  watchman watch-del-all &&
  rm -rf "$TMPDIR/react-native-packager-cache-*" &&
  rm -rf "$TMPDIR/metro-bundler-cache-*" &&
  rm -rf "$TMPDIR/haste-map-react-native-packager-*" &&
  rm -rf "node_modules/*" &&
  yarn cache clean &&
  yarn install
  '

alias ios='react-native run-ios'
alias ios4="react-native run-ios --simulator 'iPhone 4s'"
alias ios5="react-native run-ios --simulator 'iPhone 5s'"
alias ios6p="react-native run-ios --simulator 'iPhone 6 Plus'"
alias logios='react-native log-ios | ccze -A -o nolookups'

#alias avd4='$ANDROID_HOME/tools/emulator -avd Nexus_5X_API_19_x86 -gpu host -skin 1080x1920'
alias avd6='$ANDROID_HOME/tools/emulator -avd Nexus_5X_API_23_x86_64 -gpu host -skin 1080x1920'
alias avd9='$ANDROID_HOME/tools/emulator -avd Nexus_5X_API_28_x86_64 -gpu host -skin 1080x1920'
alias and='react-native run-android'
alias logand='react-native log-android | ccze -A -o nolookups'
alias build_aab='yarn run jetify && cd android && ./gradlew bundleRelease; cd ..'
alias build_apk='yarn run jetify && cd android && ./gradlew assembleRelease; cd ..'
