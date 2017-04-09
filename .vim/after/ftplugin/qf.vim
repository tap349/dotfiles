setlocal colorcolumn=
setlocal cursorline
setlocal nonumber

" https://www.reddit.com/r/vim/comments/39a9qx
" scrolloff option is global only - setlocal acts like set
let s:scrolloff = &scrolloff
set scrolloff=0

" http://vimhelp.appspot.com/vim_faq.txt.html#faq-26.8
" http://vimdoc.sourceforge.net/htmldoc/eval.html#:let-option
" :help autocmd-buffer-local
"
" don't use autocmd! in augroups with buffer-local autocommands:
" it will remove these autocommands for all other buffers.
"
" instead use autocmd! for each individual autocommand
" (or even just autocmd - buffer-local autocommand will be removed
" automatically when quickfix window is closed and ftplugin files
" are not sourced when sourcing vimrc so autocommands for the same
" buffer will be never duplicated)
augroup qf_scrolloff
  autocmd! BufLeave <buffer> let &scrolloff = s:scrolloff
  autocmd! BufEnter <buffer> set scrolloff=0
augroup END
