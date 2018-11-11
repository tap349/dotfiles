" http://vim.wikia.com/wiki/Automatic_word_wrapping
" hard wrap is used by default (newline characters are inserted)
setlocal formatoptions+=t
setlocal textwidth=79

" <CR> by default
nmap <buffer> <C-CR> <Plug>VimwikiFollowLink
" <C-Space> by default
nmap <buffer> <C-t> <Plug>VimwikiToggleListItem
