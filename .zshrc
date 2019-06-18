#===============================================================================
# ZLE configuration
#
# http://zsh.sourceforge.net/Guide/zshguide04.html:
#
# you configure it by sticking commands in your .zshrc - as it's only useful for
# an interactive shell, only /etc/zshrc and .zshrc make sense for this purpose
#===============================================================================

# https://stackoverflow.com/a/33456330/3632318
unset zle_bracketed_paste

source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
