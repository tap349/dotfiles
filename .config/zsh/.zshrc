#*******************************************************************************
#
# http://zsh.sourceforge.net/Intro/intro_3.html
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# compinit
#-------------------------------------------------------------------------------

autoload -Uz compinit

# https://github.com/sorin-ionescu/prezto/blob/master/modules/completion/init.zsh#L59
# https://gist.github.com/ctechols/ca1035271ad134841284
if [[ -n $ZDATADIR/.zcompdump(#qN.mh-20) ]]; then
  # > https://github.com/zsh-users/zsh/blob/master/Completion/compinit#L63
  # >
  # > The -C flag bypasses both the check for rebuilding the dump file and the
  # > usual call to compaudit; the -i flag causes insecure directories found by
  # > compaudit to be ignored
  #
  # Don't rebuild .zcompdump if it's modified less than 20 hours ago
  compinit -i -C
else
  # > http://zsh.sourceforge.net/Doc/Release/Completion-System.html#Use-of-compinit
  # >
  # > The dumped file is .zcompdump in the same directory as the startup files
  # > (i.e. $ZDOTDIR or $HOME); alternatively, an explicit file name can be given
  # > by ‘compinit -d dumpfile’.
  compinit -i -d $ZDATADIR/.zcompdump
fi

# Menu-style autocompletion
zstyle ':completion:*' menu select

#-------------------------------------------------------------------------------
# Kubectl autocompletion
# https://kubernetes.io/docs/reference/kubectl/cheatsheet/
#-------------------------------------------------------------------------------

[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

#-------------------------------------------------------------------------------
# ZLE configuration
#
# > http://zsh.sourceforge.net/Guide/zshguide04.html
# >
# > Hence you configure it by sticking commands in your .zshrc - as it's only
# > useful for an interactive shell, only /etc/zshrc and .zshrc make sense for
# > this purpose
#-------------------------------------------------------------------------------

# https://stackoverflow.com/a/33456330/3632318
unset zle_bracketed_paste

#-------------------------------------------------------------------------------
# Source other files
#-------------------------------------------------------------------------------

source $ZDOTDIR/bindings.zsh
# Source plugins.zsh at the end of .zshrc
source $ZDOTDIR/plugins.zsh

#-------------------------------------------------------------------------------
# mise
#-------------------------------------------------------------------------------

eval "$(mise activate zsh)"
