#*******************************************************************************
#
# https://www.reddit.com/r/zsh/comments/3ubrdr/proper_way_to_set_zdotdir/
# https://stackoverflow.com/a/46962370/3632318
#
#*******************************************************************************

# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
# http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion
export XDG_DATA_HOME=${XDG_DATA_HOME:=$HOME/.local/share}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:=$HOME/.config}

# ZDATADIR is not a standard zsh variable
export ZDATADIR=${ZDATADIR:=$XDG_DATA_HOME/zsh}
export ZDOTDIR=${ZDOTDIR:=$XDG_CONFIG_HOME/zsh}

source $ZDOTDIR/.zshenv
