# All RVM related stuff is exported in ~/.rvm/scripts/rvm which is sourced in ~/.zlogin
typeset -U path
path=(~/scripts ~/scripts/git /usr/local/bin /usr/bin /usr/sbin /bin /sbin $path)

# Set locale so that iTerm and mc could display russian letters correctly
export LANG=en_US.UTF-8
