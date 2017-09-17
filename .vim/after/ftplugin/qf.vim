setlocal colorcolumn=
setlocal cursorline
setlocal nonumber

" https://www.reddit.com/r/vim/comments/39a9qx
" scrolloff option is global only - setlocal acts like set
let s:original_scrolloff = &scrolloff
set scrolloff=0

" https://github.com/wincent/ferret/blob/master/ftplugin/qf.vim
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
  " sometimes scrolloff is not reset to its original value -
  " try to set scrolloff option directly with `set {option}`
  " (though it must be the same as using `let &{option}`)
  "autocmd! BufLeave <buffer> let &scrolloff = s:original_scrolloff
  autocmd! BufLeave <buffer> exec 'set scrolloff=' . s:original_scrolloff
  autocmd! BufEnter <buffer> set scrolloff=0
augroup END
