#-------------------------------------------------------------------------------
# http://zsh.sourceforge.net/Intro/intro_3.html
# .zshrc is sourced for interactive shells only
#-------------------------------------------------------------------------------

source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/options.zsh
source $ZDOTDIR/prompt.zsh

source $ZDOTDIR/completion.zsh
source $ZDOTDIR/bindings.zsh
source $ZDOTDIR/plugins.zsh

eval "$(mise activate zsh)"
