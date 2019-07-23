" https://stackoverflow.com/a/16987522/3632318
" https://stackoverflow.com/questions/8521846/adding-a-regular-expression-to-vimrc-with-nmap-and-imap
"
" escape `\|` to make regex work in mapping
nmap <silent><buffer> <LocalLeader>u /[^\d0-\d127]\+\([^\d0-\d127]\\|\s\)\+<CR>

"inoremap <buffer> >>t <Text></Text><Esc>F<i<CR><Esc>k$
"inoremap <buffer> >>v <View></View><Esc>F<i<CR><Esc>k$

imap <buffer> >> <><C-b>
imap <buffer> >V <View><Esc>T<
imap <buffer> >T <Text><Esc>T<
"imap <buffer> cc console.log('%c ', 'font-size: 20px; color: blue;')<Esc>T%f'i
"imap <buffer> cc console.log()
