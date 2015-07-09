typeset -U path
path=(~/scripts ~/scripts/git /usr/local/bin /usr/bin /usr/sbin /bin /sbin $path)

# for iterm to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8

#-----------------------------------------------------------------------------------------
# Load RVM into a shell session *as a function*
# https://github.com/scrooloose/syntastic/issues/1310
#-----------------------------------------------------------------------------------------

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
