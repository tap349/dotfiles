#-------------------------------------------------------------------------------
# compinit
#-------------------------------------------------------------------------------

# Homebrew installs completions (_kubectl, _mise, etc.) here,
# but Apple's zsh doesn't search this dir by default. No need
# to source app-specific completions manually
fpath=(/opt/homebrew/share/zsh/site-functions $fpath)

autoload -Uz compinit

# https://github.com/sorin-ionescu/prezto/blob/master/modules/completion/init.zsh#L59
# https://gist.github.com/ctechols/ca1035271ad134841284
#
# > https://github.com/zsh-users/zsh/blob/master/Completion/compinit#L63
# >
# > The -C flag bypasses both the check for rebuilding the dump file and the
# > usual call to compaudit; the -i flag causes insecure directories found by
# > compaudit to be ignored
#
# > http://zsh.sourceforge.net/Doc/Release/Completion-System.html#Use-of-compinit
# >
# > The dumped file is .zcompdump in the same directory as the startup files
# > (i.e. $ZDOTDIR or $HOME); alternatively, an explicit file name can be given
# > by ‘compinit -d dumpfile’.
if [[ -n $ZDATADIR/.zcompdump(#qN.mh-20) ]]; then
  # Don't rebuild .zcompdump if it's modified less than 20 hours ago
  compinit -i -C -d $ZDATADIR/.zcompdump
else
  compinit -i -d $ZDATADIR/.zcompdump
  # compinit leaves the dump untouched when it's already up to date,
  # so bump mtime ourselves or the fast path above is never taken
  touch $ZDATADIR/.zcompdump
fi

# Menu-style autocompletion
zstyle ':completion:*' menu select
