#-----------------------------------------------------------------------------------------
# Path to your oh-my-zsh configuration.

ZSH=$HOME/.oh-my-zsh

#-----------------------------------------------------------------------------------------
# Look in ~/.oh-my-zsh/themes/
# Theme previews can be found at https://github.com/robbyrussell/oh-my-zsh/wiki/themes

ZSH_THEME="tap-af-magic"

#-----------------------------------------------------------------------------------------
# Set to this to use case-sensitive completion

CASE_SENSITIVE="true"

#-----------------------------------------------------------------------------------------
# Uncomment following line if you want red dots to be displayed while waiting for completion

COMPLETION_WAITING_DOTS="true"

#-----------------------------------------------------------------------------------------
# Uncomment following line if you want to show in the command execution time stamp
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd

HIST_STAMPS="yyyy-mm-dd"

#-----------------------------------------------------------------------------------------
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

# https://github.com/rails/spring/tree/v0.0.9#usage
UNBUNDLED_COMMANDS=(spring)
plugins=(bundler ssh-agent)

source $ZSH/oh-my-zsh.sh

#=========================================================================================
#
# User configuration
#
#=========================================================================================

#-----------------------------------------------------------------------------------------
#
# Environment variables
#
#-----------------------------------------------------------------------------------------

TERM="xterm-256color"

HISTSIZE=100000
HISTFILESIZE=200000
setopt HIST_IGNORE_DUPS

CDPATH=~:~/dev

BLOG=~/blog

CHEF=~/dev/chef
PUMBA=~/dev/pumba
SHIKIMORI=~/dev/shikimori
UPTIMUS=~/dev/uptimus

#-----------------------------------------------------------------------------------------
#
# Aliases
#
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# cd
#-----------------------------------------------------------------------------------------

alias dotfiles='cd ~/.dotfiles'
alias downloads='cd ~/Downloads'

alias blog='cd $BLOG'

alias chef='cd $CHEF'
alias pumba='cd $PUMBA'
alias shikimori='cd $SHIKIMORI'
alias uptimus='cd $UPTIMUS'

alias c='cd $CHEF'
alias p='cd $PUMBA'
alias s='cd $SHIKIMORI'
alias u='cd $UPTIMUS'

#-----------------------------------------------------------------------------------------
# common
#-----------------------------------------------------------------------------------------

alias df='df -h'
alias ll='ls -alp'
alias mail='less +G /var/mail/tap'

# for octopress
alias rake="noglob rake"

#-----------------------------------------------------------------------------------------
# dev
#-----------------------------------------------------------------------------------------

# blog

alias upload='rake generate && rake deploy'

# cap

alias pdeploy='git push && cap production deploy'
alias sdeploy='git push && cap staging deploy'
alias deploy='sdeploy'

# git

alias g='git'
alias ga='git add -A .'
alias gl="git log --graph --pretty=format:'%Cred%h%Creset %C(yellow)%d%Creset %s - %C(bold blue)%an%Creset, %Cgreen%cr' --abbrev-commit"
alias gs='git status'
alias gbd='git_delete_branches'

# hitch

alias unhitch='hitch -u'

# misc

alias log='tail -f log/development.log'
alias migrate='rake db:migrate && RAILS_ENV=test rake db:migrate'
alias rollback='rake db:rollback STEP=1 && RAILS_ENV=test rake db:rollback STEP=1'
alias sass='sass-convert --from css --to sass -R'
alias sidekiq='bundle exec sidekiq --config ./config/sidekiq.yml'

# rails

alias r='rails'
alias rc='rails console'
alias rd='rails dbconsole'
alias rg='rails generate'
alias rs='rails server'

# SSH
#
# it's to possible to SSH in 2 ways:
#
# - ssh <host from .ssh/config>
# - ssh <username>@<host from /etc/hosts>
#
# in the 1st case user is supplied from .ssh/config
#
# in both cases public key .ssh/id_rsa.pub is used to authenticate
# if it has been added to .ssh/authorized_keys on server for specified user

alias caravan="ssh caravan"
alias linode-uptimus="ssh linode-uptimus"
alias linode-pumba="ssh linode-pumba"
alias tap349="ssh tap349"

#-----------------------------------------------------------------------------------------
#
# Functions
#
#-----------------------------------------------------------------------------------------

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

git_delete_branches() {
  git branch | grep -v -E '(master|develop)' | xargs git branch -d
}

gr() {
  fgrep --color --exclude-dir={log,public,tmp,.git} -Iir "$@" .
}

hitch() {
  command hitch "$@"
  if [[ -s "$HOME/.hitch_export_authors" ]] ; then source "$HOME/.hitch_export_authors" ; fi
}

orig() {
  find . -iname '*.orig' -exec rm {} \;
}
