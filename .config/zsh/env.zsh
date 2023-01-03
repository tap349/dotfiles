#*******************************************************************************
#
# Environment variables
#
#*******************************************************************************

export EDITOR=mvim
export PAGER=less
export TERM='xterm-256color'
# https://github.com/keybase/keybase-issues/issues/1712#issuecomment-141226705
# for `gpg` to sign commits
export GPG_TTY=$(tty)

#-------------------------------------------------------------------------------
# Zsh options
#
# http://zsh.sourceforge.net/Doc/Release/Options.html
#-------------------------------------------------------------------------------

# https://mattprice.me/2015/zsh-path-issues-osx-el-capitan/
# don't run /etc/zprofile - it might reorder directories in PATH
setopt NO_GLOBAL_RCS
# http://zsh.sourceforge.net/Intro/intro_2.html
# say, to be able to use case-insensitive globbing (#i)
setopt EXTENDED_GLOB

#-------------------------------------------------------------------------------
# PATH
#-------------------------------------------------------------------------------

# removes duplicate entries from PATH
typeset -U path
path=($HOME/scripts $path)
# Homebrew creates symlinks in
# - /usr/local/bin/ for Intel
# - /opt/homebrew/bin/ for Apple Silicon
path=(/usr/local/bin /opt/homebrew/bin $path)
# > `brew info libpq`
# >
# > libpq is keg-only, which means it was not symlinked into /usr/local,
# > because conflicts with postgres formula.
#path=(/usr/local/opt/libpq/bin $path)
path=(/opt/homebrew/opt/libpq/bin $path)
path=($HOME/soft/google-cloud-sdk/bin $path)
path=(/Library/Java/JavaVirtualMachines/graalvm-ce-java19-22.3.0/Contents/Home/bin $path)
path=($HOME/bin $path)

#-------------------------------------------------------------------------------
# Prompt
#-------------------------------------------------------------------------------

# http://zsh.sourceforge.net/Doc/Release/Expansion.html#Command-Substitution
# for command substitution to work in prompt
setopt prompt_subst

# https://wiki.archlinux.org/index.php/Zsh#Colors
PS1='\
%K{#1F2F40}\
%F{032} %~ %F{reset}\
$(git_prompt)\
%K{reset}\
%F{032} %(!.#.») %F{reset}'

#-------------------------------------------------------------------------------
# Locale
#-------------------------------------------------------------------------------

# for iTerm2 to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8

#-------------------------------------------------------------------------------
# Zsh history
#
# https://stackoverflow.com/a/19454838/3632318
#-------------------------------------------------------------------------------

# http://zsh.sourceforge.net/Doc/Release/Options.html
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt INC_APPEND_HISTORY_TIME

HISTFILE=$ZDATADIR/.zsh_history
HISTFILESIZE=10000
HISTSIZE=10000
# http://zsh.sourceforge.net/Guide/zshguide02.html
# history is not persisted if SAVEHIST is not set
SAVEHIST=10000

#-------------------------------------------------------------------------------
# Elixir
#-------------------------------------------------------------------------------

# https://hexdocs.pm/iex/IEx.html#module-shell-history
export ERL_AFLAGS='-kernel shell_history enabled'

#-------------------------------------------------------------------------------
# Source other files
#-------------------------------------------------------------------------------

source $ZDOTDIR/env.secret.zsh
