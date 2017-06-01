"-------------------------------------------------------------------------------
" open alternate files
" (instead of using :A/:AV from vim-rails/vim-rake/phoenix.vim plugins)
"-------------------------------------------------------------------------------

" AltCommand function is defined ni mappings.vim
nnoremap <buffer> <silent> <Leader>, :call AltCommand(expand('%'), ':e')<CR>
nnoremap <buffer> <silent> <Leader>v :call AltCommand(expand('%'), ':vs')<CR>
