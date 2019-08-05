"*******************************************************************************
"
" autocommands
"
" exclamation mark means to remove all autocommands associated with event,
" pattern and command - the point here is that by default autocommands are
" are accumulated every time .vimrc is sourced.
"
"*******************************************************************************

augroup backup
  autocmd!
  autocmd BufWritePre * call s:SetBackupDir()
augroup END

let s:prevtabnr = tabpagenr()
let s:prevtabcount = tabpagenr('$')
augroup tabs
  autocmd!
  " go to previous tab after closing tab
  autocmd TabEnter * call s:GoToPrevTab()
  " to be able to switch to last active tab - see mappings
  autocmd TabLeave * let g:lasttabnr = tabpagenr()
augroup END

" some colors might be broken after sourcing vimrc in autocmd,
" to name a few:
"
" - sign column color (SignColumn highlight group) which is
"   used by vim-gitgutter plugin but not defined explicitly
"   in github colorscheme
" - RGB values become colorless if `syntax on` is commented out
"   in vimrc at the same time
"augroup vimrc
"  autocmd!
"  autocmd BufWritePost $MYVIMRC source $MYVIMRC | call lightline#enable()
"  autocmd BufWritePost *.vim source $MYVIMRC | call lightline#enable()
"augroup END

" http://vim.wikia.com/wiki/Highlight_unwanted_spaces#Resolving_performance_problems
" fix possible memory leaks
augroup matches
  autocmd!
  autocmd BufWinLeave * call clearmatches()
augroup END

" used to disable <CR> in quickfix window - now it's
" not necessary because QFEnter mapping already does it
"augroup quickfix
"  autocmd!
"  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
"  " http://stackoverflow.com/a/13813231
"  autocmd QuickFixCmdPre * let g:winview = winsaveview()
"  autocmd QuickFixCmdPost * call winrestview(g:winview)
"augroup END

"===============================================================================
"
" functions for autocommands
"
"===============================================================================

" IDK how but expand('%:p:h') might expand into relative paths which
" would result into weird directories like backupapp, backupcookbooks,
" backupdeps and backuplib (no files are actually written there) =>
" it's safe to add extra slash before expanded path to avoid it
function! s:SetBackupDir()
  " don't use ~ for home directory - it's not expanded
  let l:backupdir = $HOME . '/.vim/backup/' . expand('%:p:h')

  if !isdirectory(l:backupdir)
    call mkdir(l:backupdir, 'p', 0700)
  endif

  let &backupdir = l:backupdir
  " new files overwrite existing ones
  "let &backupext = '-' . strftime('(%Y-%m-%d %H:%M:%S)') . '~'
  let &backupext = '-' . strftime('(%Y-%m-%d %H:%M)') . '~'
endfunction

" http://stackoverflow.com/questions/14079149
function! s:GoToPrevTab()
  if tabpagenr('$') < s:prevtabcount && tabpagenr() > 1 && tabpagenr() == s:prevtabnr
    tabprevious
  endif

  let s:prevtabnr = tabpagenr()
  let s:prevtabcount = tabpagenr('$')
endfunction

" http://stackoverflow.com/a/8459043
" returns 1 (true) if buffer hidden or 0 (false) otherwise
"function! BufHidden(buf)
"  let active_buffers = []
"  let tabs = range(1, tabpagenr('$'))

"  call map(tabs, 'extend(active_buffers, tabpagebuflist(v:val))')
"  return (bufexists(a:buf) && index(active_buffers, a:buf) == -1)
"endfunction

" a:000: http://learnvimscriptthehardway.stevelosh.com/chapters/24.html
" sbuffer: `buffer` function doesn't respect `switchbuf` option
"function! GotoOrOpenTab(...)
"  for file in a:000
"    " if buffer exists and not hidden
"    if bufexists(file) && !BufHidden(bufnr(file))
"      exec 'sbuffer ' . file
"    else
"      exec 'tabedit ' . file
"    endif
"  endfor
"endfunction
