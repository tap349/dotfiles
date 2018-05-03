" https://robots.thoughtbot.com/writing-vim-syntax-plugins#syntax
" https://stackoverflow.com/a/15702132/3632318
" :help syn-match
hi Typespec guifg=#A0A0B0 gui=italic
syn match Typespec /^\s*@spec.*$/
