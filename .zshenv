# All RVM related staff is exported in ~/.rvm/scripts/rvm which is sourced in ~/.zlogin
typeset -U path
path=(~/scripts /usr/local/bin /usr/bin /usr/sbin /bin /sbin $path)

# Set locale so that iTerm could display russian letters correctly
LANG="ru_RU.UTF-8"
