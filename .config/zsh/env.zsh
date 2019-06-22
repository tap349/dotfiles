#*******************************************************************************
#
# environment variables
#
#*******************************************************************************

export EDITOR=mvim
export PAGER=less
export TERM='xterm-256color'
# https://github.com/keybase/keybase-issues/issues/1712#issuecomment-141226705
# for `gpg` to sign commits
export GPG_TTY=$(tty)

#-------------------------------------------------------------------------------
# zsh options
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
# brew creates symlinks in /usr/local/bin/
path=(/usr/local/bin $path)

#-------------------------------------------------------------------------------
# prompt
#-------------------------------------------------------------------------------

# http://zsh.sourceforge.net/Doc/Release/Expansion.html#Command-Substitution
# for command substitution to work in prompt
setopt prompt_subst

# https://wiki.archlinux.org/index.php/Zsh#Colors
PS1='\
%F{032}%~ \
$(git_prompt)\
%F{032}%(!.#.») \
%F{reset}'

#-------------------------------------------------------------------------------
# locale
#-------------------------------------------------------------------------------

# for iTerm2 to display cyrillic
export LANG=en_US.UTF-8
# for mc to display cyrillic
export LC_ALL=en_US.UTF-8

#-------------------------------------------------------------------------------
# zsh history
#-------------------------------------------------------------------------------

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

# https://stackoverflow.com/a/19454838/3632318
HISTFILE=$ZDATADIR/.zsh_history
HISTFILESIZE=10000
HISTSIZE=10000

#-------------------------------------------------------------------------------
# homebrew
#-------------------------------------------------------------------------------

export HOMEBREW_GITHUB_API_TOKEN=03adebc410e1f8de5a2765a5f5890ff8beb76d5f

#-------------------------------------------------------------------------------
# Elixir
#-------------------------------------------------------------------------------

# https://hexdocs.pm/iex/IEx.html#module-shell-history
export ERL_AFLAGS='-kernel shell_history enabled'

#-------------------------------------------------------------------------------
# React Native
#-------------------------------------------------------------------------------

# https://github.com/jhen0409/react-native-debugger/blob/master/docs/getting-started.md#launch-by-cli-or-react-native-packager-macos-only
# https://github.com/artsy/emission/blob/45417ca425f2cba7d2da21902ef8ff1cd093a024/package.json#L28
#
# set React Native Debugger as JavaScript debugger
export REACT_DEBUGGER="open -g 'rndebugger://set-debugger-loc?port=8081' --args"

# https://github.com/bradmartin/nativescript-videoplayer/issues/76
export SIMCTL_CHILD_OS_ACTIVITY_MODE='disable'

# required by `react-native-cli` to be set
export ANDROID_HOME=/usr/local/share/android-sdk
# `android`
path=($path $ANDROID_HOME/tools)
# `sdkmanager` and `avdmanager`
path=($path $ANDROID_HOME/tools/bin)
# `adb`
path=($path $ANDROID_HOME/platform-tools)
