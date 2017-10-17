#-----------------------------------------------------------------------------------------
# set to this to use case-sensitive completion
#-----------------------------------------------------------------------------------------

CASE_SENSITIVE='true'

#-----------------------------------------------------------------------------------------
# uncomment following line if you want red dots to be displayed while waiting for completion
#-----------------------------------------------------------------------------------------

# NOTE don't use along with zsh-autosuggestions!
COMPLETION_WAITING_DOTS='false'

#-----------------------------------------------------------------------------------------
# uncomment following line if you want to show in the command execution time stamp
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
#-----------------------------------------------------------------------------------------

HIST_STAMPS='yyyy-mm-dd'

#=========================================================================================
#
# oh-my-zsh
#
#=========================================================================================

#-----------------------------------------------------------------------------------------
# themes: ~/.oh-my-zsh/themes/
# theme previews can be found at https://github.com/robbyrussell/oh-my-zsh/wiki/themes
#-----------------------------------------------------------------------------------------

ZSH_THEME='tap-af-magic'

#-----------------------------------------------------------------------------------------
# plugins: ~/.oh-my-zsh/plugins/
# custom plugins (ZSH_CUSTOM): ~/.oh-my-zsh/custom/plugins/
#-----------------------------------------------------------------------------------------

# https://github.com/rails/spring/tree/v0.0.9#usage
UNBUNDLED_COMMANDS=(spring)
plugins=(ssh-agent gpg-agent zsh-autosuggestions zsh-syntax-highlighting)

# https://github.com/robbyrussell/oh-my-zsh/tree/master/plugins/ssh-agent
zstyle :omz:plugins:ssh-agent agent-forwarding on

#-----------------------------------------------------------------------------------------
# source oh-my-zsh.sh after all configuration is done
#-----------------------------------------------------------------------------------------

ZSH=$HOME/.oh-my-zsh
DISABLE_AUTO_UPDATE=true

source $ZSH/oh-my-zsh.sh

#=========================================================================================
#
# user configuration
#
#=========================================================================================

#-----------------------------------------------------------------------------------------
#
# http://unix.stackexchange.com/a/71258
#
# environment variables, aliases and functions are moved to ~/.zshenv
# because it's loaded for all shells (not only interactive ones) -
# for example, it allows execute zsh functions in vim shell
#
# also rbenv initialization is moved to ~/.zshenv as well
#
#-----------------------------------------------------------------------------------------

# define overwritten by oh-my-zsh aliases from ~/.zshenv
# once again here (must be done after sourcing oh-my-zsh)

unalias ll
alias ll='ls -alp'
