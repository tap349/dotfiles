#-------------------------------------------------------------------------------
# http://zsh.sourceforge.net/Guide/zshguide04.html
# Use `cat -v` to find escape sequences
#-------------------------------------------------------------------------------

# Emacs mode
bindkey -e

bindkey '^R' history-incremental-search-backward

# https://stackoverflow.com/a/41885766
bindkey "^[[3~" delete-char

# Treat Ctrl+Enter as normal Enter at the shell prompt
bindkey '^[[27;5;13~' accept-line
