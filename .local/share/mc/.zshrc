# Midnight Commander uses this file for Zsh subshell instead of ~/.zshrc.
# Inside Midnight Commander $ZDOTDIR = ~/.local/share/mc

source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/options.zsh"
source "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/aliases.zsh"

PS1='%~ $ '
