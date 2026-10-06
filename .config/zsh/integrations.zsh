#-------------------------------------------------------------------------------
# OrbStack
#
# Keep before mise: it appends to PATH, and mise skips `mise hook-env`
# (executed in _mise_hook_precmd) at first prompt only if PATH is
# unchanged since activation. Not relevant now since _mise_hook_precmd
# is unregistered below, but still harmless to keep
#-------------------------------------------------------------------------------

source ~/.orbstack/shell/init.zsh 2>/dev/null || :

#-------------------------------------------------------------------------------
# fzf
#-------------------------------------------------------------------------------

source <(fzf --zsh)

export FZF_CTRL_R_OPTS="
  --border
  --exact
  --height 14
  --layout reverse
"

#-------------------------------------------------------------------------------
# mise
#-------------------------------------------------------------------------------

eval "$(mise activate zsh)"

# mise runs `mise hook-env` before every prompt which costs ~260 ms =>
# unregister precmd hook, keep only chpwd hook (edits to mise.toml in
# the current dir are picked up after `cd .`)
#
# NOTE: Relies on an internal mise function name: if it's renamed, this
# is a silent no-op and prompts get slow again
add-zsh-hook -d precmd _mise_hook_precmd
