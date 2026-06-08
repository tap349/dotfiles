export EDITOR=vim
export PAGER=less

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# https://github.com/keybase/keybase-issues/issues/1712#issuecomment-141226705
# For gpg to sign commits
export GPG_TTY=$(tty)

# Don't use proxy or checksum database for private modules
export GOPRIVATE=github.com/inDriver

# By default context configs are stored in ~/.local/share/k9s/clusters
# (see `k9s info`)
export K9S_CONFIG_DIR=$HOME/.config/k9s

# Disable trust checks
export HOMEBREW_NO_REQUIRE_TAP_TRUST=1

source $ZDOTDIR/env.secret.zsh

#-------------------------------------------------------------------------------
# PATH
#-------------------------------------------------------------------------------

# Remove duplicate entries from PATH
typeset -U path

path=($HOME/scripts $path)

# Homebrew
path=(/opt/homebrew/bin /opt/homebrew/sbin $path)

# Dart
path=($HOME/.pub-cache/bin $path)

# go env GOPATH => $HOME/go
path=($HOME/go/bin $path)

# mysql
path=(/opt/homebrew/opt/mysql-client/bin $path)

# psql
path=(/opt/homebrew/opt/libpq/bin $path)

# For telepresence-oss
path=(/usr/local/bin $path)
