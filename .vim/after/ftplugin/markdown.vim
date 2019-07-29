" http://vim.wikia.com/wiki/Automatic_word_wrapping
" hard wrap is used by default (newline characters are inserted)
setlocal formatoptions+=t
setlocal textwidth=79

iabbrev <buffer> -> →
" http://vim.wikia.com/wiki/Multi-line_abbreviations
iabbrev <buffer> dl 
      \<dl>
      \<CR>  <dt></dt>
      \<CR><dd></dd>
      \<CR></dl>
      \<Esc><<A
      \<CR>
      \<CR><hr>
      \<Esc>6k^

nmap <buffer> <LocalLeader>p :!publish<CR>
" turn current line into h2 header
" UPDATE: not used because of prettier
"nmap <buffer> <LocalLeader>h :normal! mmyypVr-`m<CR>
nmap <buffer> <LocalLeader>i o<!-- prettier-ignore --><ESC>
