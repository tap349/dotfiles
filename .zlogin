#-----------------------------------------------------------------------------------------
# http://zsh.sourceforge.net/Intro/intro_3.html
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# MacVim defaults
#
# https://github.com/macvim-dev/macvim/wiki/FAQ
# :h macvim-prefs
#
# most preferences are boolean values:
# both 0/1 and false/true are accepted
#
# reset current preferences:
# $ defaults delete org.vim.MacVim
#
# show current preferences:
# $ defaults read org.vim.MacVim
#-----------------------------------------------------------------------------------------

defaults write org.vim.MacVim MMShowAddTabButton 0
defaults write org.vim.MacVim MMNoTitleBarWindow 1
defaults write org.vim.MacVim MMZoomBoth 1
defaults write org.vim.MacVim MMTextInsetTop 0
defaults write org.vim.MacVim MMTextInsetRight 5
defaults write org.vim.MacVim MMTextInsetBottom 0
defaults write org.vim.MacVim MMTextInsetLeft 5
defaults write org.vim.MacVim MMFullScreenFadeTime 0
# https://github.com/macvim-dev/macvim/wiki/FAQ#black-screen-on-full-screen
defaults write org.vim.MacVim MMUseCGLayerAlways 1
# https://github.com/macvim-dev/macvim/issues/390#issuecomment-254252969
defaults write org.vim.MacVim SUEnableAutomaticChecks 0
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
