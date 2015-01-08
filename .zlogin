#-----------------------------------------------------------------------------------------
#
# http://zsh.sourceforge.net/Intro/intro_3.html
#
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Load RVM into a shell session *as a function*
#-----------------------------------------------------------------------------------------

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

#-----------------------------------------------------------------------------------------
# MacVim defaults
#
# https://code.google.com/p/macvim/wiki/UserDefaults
# :h macvim-prefs
#-----------------------------------------------------------------------------------------

defaults write org.vim.MacVim MMShowAddTabButton false
defaults write org.vim.MacVim MMTabMinWidth 150
defaults write org.vim.MacVim MMTabMaxWidth 250
defaults write org.vim.MacVim MMTabOptimumWidth 200
