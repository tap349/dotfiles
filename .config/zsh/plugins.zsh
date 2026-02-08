#*******************************************************************************
#
# Plugins
#
#*******************************************************************************

#-------------------------------------------------------------------------------
# zsh-autosuggestions
#
# git clone https://github.com/zsh-users/zsh-autosuggestions $ZDATADIR/zsh-autosuggestions
#-------------------------------------------------------------------------------

ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_MANUAL_REBIND=1
ZSH_AUTOSUGGEST_USE_ASYNC=1

source $ZDATADIR/zsh-autosuggestions/zsh-autosuggestions.zsh

#-------------------------------------------------------------------------------
# zsh-syntax-highlighting
#
# git clone https://github.com/zsh-users/zsh-syntax-highlighting $ZDATADIR/zsh-syntax-highlighting
#
# > https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md#in-your-zshrc
# >
# > Note the source command must be at the end of ~/.zshrc.
#-------------------------------------------------------------------------------

source $ZDATADIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#-------------------------------------------------------------------------------
# zsh-kubectl-prompt
#
# git clone https://github.com/superbrothers/zsh-kubectl-prompt $ZDATADIR/zsh-kubectl-prompt
#-------------------------------------------------------------------------------

autoload -U colors; colors
source $ZDATADIR/zsh-kubectl-prompt/kubectl.zsh

function right_prompt() {
  local color="blue"

  if [[ "$ZSH_KUBECTL_CONTEXT" == "prod-eks-cluster" ||
          "$ZSH_KUBECTL_CONTEXT" == "dev-platform-eks-prod" ||
          "$ZSH_KUBECTL_CONTEXT" == "gke_infra-ng" ]]; then
    color=red
  fi

  echo "%{$fg[$color]%}($ZSH_KUBECTL_CONTEXT)%{$reset_color%}"
}

RPROMPT='$(right_prompt)'
