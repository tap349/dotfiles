#-----------------------------------------------------------------------------------------
# http://zsh.sourceforge.net/Intro/intro_3.html
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# MacVim defaults
#
# https://code.google.com/p/macvim/wiki/UserDefaults
# :h macvim-prefs
#-----------------------------------------------------------------------------------------

defaults write org.vim.MacVim MMShowAddTabButton false
defaults write org.vim.MacVim MMNoTitleBarWindow true
defaults write org.vim.MacVim MMZoomBoth true
defaults write org.vim.MacVim MMTextInsetTop 0
defaults write org.vim.MacVim MMTextInsetRight 5
defaults write org.vim.MacVim MMTextInsetBottom 0
defaults write org.vim.MacVim MMTextInsetLeft 5
defaults write org.vim.MacVim MMFullScreenFadeTime 0
#defaults write org.vim.MacVim MMNoFontSubstitution 1
#defaults write org.vim.MacVim MMNativeFullScreen 1
#defaults write org.vim.MacVim MMTabMinWidth 120
#defaults write org.vim.MacVim MMTabMaxWidth 250
#defaults write org.vim.MacVim MMTabOptimumWidth 200

#-----------------------------------------------------------------------------------------
# PATH
#-----------------------------------------------------------------------------------------

# add /opt/chefdk/bin to PATH here because ~/.zlogin is sourced
# after ~/.zshrc where shims directory is prepended to PATH
#
# UPDATE: use `chef exec` instead
#path=(/opt/chefdk/bin $path)
