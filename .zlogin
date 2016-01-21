#-----------------------------------------------------------------------------------------
# http://zsh.sourceforge.net/Intro/intro_3.html
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# MacVim defaults
#
# https://code.google.com/p/macvim/wiki/UserDefaults
# :h macvim-prefs
#-----------------------------------------------------------------------------------------

defaults write org.vim.MacVim MMNoTitleBarWindow true
defaults write org.vim.MacVim MMShowAddTabButton false
defaults write org.vim.MacVim MMZoomBoth false
#defaults write org.vim.MacVim MMTabMinWidth 120
#defaults write org.vim.MacVim MMTabMaxWidth 250
#defaults write org.vim.MacVim MMTabOptimumWidth 200

# don't source rvm in ~/.zshenv because in that case rvm
# doesn't switch to its default ruby in new zsh session
#
# source rvm twice:
#   1) in ~/.zlogin (here) for zsh
#   2) in ~/.profile for macvim
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
