" http://vimhelp.appspot.com/vim_faq.txt.html#faq-26.8
" NOTE: don't source this file from vimrc - it's sourced automatically

if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect
  autocmd! BufRead,BufNewFile *.arb,*.jb setfiletype ruby
  autocmd! BufRead,BufNewFile *.service,*.socket,*.timer setfiletype sh
  " using rspec filetype for specs doesn't change highlighting
  " at all but prevents rubocop checker from running on them
  "autocmd! BufRead,BufNewFile *_spec.rb setfiletype rspec
augroup END
