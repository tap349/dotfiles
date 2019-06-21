#*******************************************************************************
#
# http://zsh.sourceforge.net/Intro/intro_3.html
# http://unix.stackexchange.com/a/71258
# https://github.com/joshtronic/dotfiles/blob/master/zshrc
# https://htr3n.github.io/2018/07/faster-zsh/
#
# environment variables, aliases and functions are sourced in .zshenv because
# the latter is loaded for all shells (not only interactive ones) - it allows
# to execute zsh functions in vim shell as well
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# source other files
#-------------------------------------------------------------------------------

# source functions.zsh before aliases.zsh since aliases might use functions
source $ZDOTDIR/env.zsh
source $ZDOTDIR/functions.zsh
source $ZDOTDIR/aliases.zsh
