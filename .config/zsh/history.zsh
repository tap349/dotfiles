# https://stackoverflow.com/a/19454838/3632318
# http://zsh.sourceforge.net/Doc/Release/Options.html
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt INC_APPEND_HISTORY_TIME

HISTFILE=$ZDATADIR/.zsh_history
HISTFILESIZE=10000
HISTSIZE=10000
# http://zsh.sourceforge.net/Guide/zshguide02.html
# History is not persisted if SAVEHIST is not set
SAVEHIST=10000
