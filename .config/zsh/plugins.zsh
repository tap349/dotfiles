#*******************************************************************************
#
# Plugins
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# zsh-autosuggestions
#-------------------------------------------------------------------------------

ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_MANUAL_REBIND=1
ZSH_AUTOSUGGEST_USE_ASYNC=1

source $ZDATADIR/zsh-autosuggestions/zsh-autosuggestions.zsh

#-------------------------------------------------------------------------------
# zsh-syntax-highlighting
#
# > https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md#in-your-zshrc
# >
# > Note the source command must be at the end of ~/.zshrc.
#-------------------------------------------------------------------------------

source $ZDATADIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
