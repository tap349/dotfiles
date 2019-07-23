" https://robots.thoughtbot.com/writing-vim-syntax-plugins#syntax
" https://stackoverflow.com/questions/15701808/vim-custom-highlighting-for-lines-starting-with/15702132#15702132
" https://stackoverflow.com/questions/18391665/vim-positive-lookahead-regex
" :help syn-region
"syn match Typespec /^\s*@spec.*$/
syn region Typespec start=/^\s*@spec/ skip=/$/ end=/\(^\s*def\)\@=/
hi Typespec guifg=#A0A0B0 gui=italic
