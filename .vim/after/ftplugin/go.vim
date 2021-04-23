set nolist

" https://ieftimov.com/post/testing-in-go-go-test/
" Use package list mode (./...) to enable caching
nmap <buffer> <C-t> :!cd %:p:h && go test ./...<CR>
