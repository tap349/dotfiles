iabbrev -> →
" http://vim.wikia.com/wiki/Multi-line_abbreviations
iabbrev dl 
      \<dl>
      \<CR>  <dt></dt>
      \<CR><dd></dd>
      \<CR></dl>
      \<Esc><<4k^

nmap <buffer> <LocalLeader>p :!publish<CR>
nmap <buffer> <LocalLeader>g :Goyo<CR>
" turn current line into h2 header
nmap <buffer> <LocalLeader>h :normal! mmyypVr-`m<CR>
