setlocal colorcolumn=

augroup my_extradite
  autocmd!
  autocmd BufLeave <buffer> setlocal colorcolumn=81
augroup END
