# all RVM related stuff is exported in ~/.rvm/scripts/rvm which is sourced in ~/.zlogin
typeset -U path
path=(~/scripts ~/scripts/git /usr/local/bin /usr/bin /usr/sbin /bin /sbin $path)

# for iterm to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8
