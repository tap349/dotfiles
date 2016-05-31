# don't use /etc/zprofile - it might reorder directories in PATH
# https://mattprice.me/2015/zsh-path-issues-osx-el-capitan/
setopt no_global_rcs

# NOTE: add /opt/chefdk/bin to PATH in ~/.zlogin
typeset -U path
path=(~/scripts /usr/local/bin $path)

# for iterm to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8
