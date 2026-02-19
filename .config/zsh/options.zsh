#-------------------------------------------------------------------------------
# http://zsh.sourceforge.net/Doc/Release/Options.html
#-------------------------------------------------------------------------------

# https://mattprice.me/2015/zsh-path-issues-osx-el-capitan/
# Don't run /etc/zprofile - it might reorder directories in PATH
setopt NO_GLOBAL_RCS

# http://zsh.sourceforge.net/Intro/intro_2.html
# Say, to be able to use case-insensitive globbing (#i)
setopt EXTENDED_GLOB

# https://stackoverflow.com/a/33456330/3632318
unset zle_bracketed_paste
