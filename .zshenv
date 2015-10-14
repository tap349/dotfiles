typeset -U path
path=(~/.rvm/bin ~/scripts ~/scripts/git /usr/local/bin /usr/bin /usr/sbin /bin /sbin $path)

# for iterm to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8

#-----------------------------------------------------------------------------------------
# https://rvm.io/integration/vim
# https://github.com/scrooloose/syntastic/issues/1407
#-----------------------------------------------------------------------------------------

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
