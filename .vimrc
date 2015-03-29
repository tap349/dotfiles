"********************************************************************************
"                                                                               *
" NOTE: these keys generate the same characters:                                *
"       - <Tab> and <C-i>                                                       *
"       - <CR> and <C-m>                                                        *
"       - <Esc> and <C-[>                                                       *
"                                                                               *
" USEFUL RESOURCES:                                                             *
"                                                                               *
" http://www.ibm.com/developerworks/library/l-vim-script-1/                     *
"                                                                               *
"********************************************************************************

"================================================================================
"                                                                               =
" compatibility with morr's configuration                                       =
"                                                                               =
" http://www.arl.wustl.edu/~fredk/Courses/Docs/vim/options.html                 =
" http://superuser.com/questions/224607                                         =
"                                                                               =
"================================================================================

source ~/.dotfiles/.vimrc.morr.basic

"================================================================================
"                                                                               =
" pathogen                                                                      =
"                                                                               =
" https://github.com/tpope/vim-pathogen                                         =
" http://logicalfriday.com/2011/07/18/using-vim-with-pathogen/                  =
" http://www.vim.org/scripts/script.php?script_id=2332                          =
"                                                                               =
"================================================================================

execute pathogen#infect()
syntax on
filetype plugin indent on

"================================================================================
"                                                                               =
" performance tweaks                                                            =
"                                                                               =
" http://stackoverflow.com/questions/16902317                                   =
"                                                                               =
"================================================================================

"set synmaxcol=200
set regexpengine=2

"================================================================================
"                                                                               =
" general settings                                                              =
"                                                                               =
"================================================================================

set nocompatible
set nowrap

"-------------------------------------------------------------------------------
" backup and swap files
"-------------------------------------------------------------------------------

set backup

set backupdir=~/.vim/backup//
set directory=~/.vim/tmp//

" http://vim.wikia.com/wiki/Editing_crontab
set backupskip+=/private/tmp/*

"-------------------------------------------------------------------------------
" buffers
"-------------------------------------------------------------------------------

" http://vim.wikia.com/wiki/Using_tab_pages
"set switchbuf=usetab,newtab
set hidden

"-------------------------------------------------------------------------------
" completion menu
"-------------------------------------------------------------------------------

set wildmenu
set wildmode=longest:full,full

"-------------------------------------------------------------------------------
" indentation
"-------------------------------------------------------------------------------

" dumb auto indentation - copy indentation from previous line
" it's automatically enabled with plugin-specific indentation on
"set autoindent

set expandtab
set shiftround
set shiftwidth=2
set softtabstop=2
set tabstop=2

"-------------------------------------------------------------------------------
" visualize invisible characters
"-------------------------------------------------------------------------------

set list
set listchars=nbsp:·,tab:▸·,trail:·,precedes:«,extends:»

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

set scrolloff=10

"-------------------------------------------------------------------------------
" search
"-------------------------------------------------------------------------------

set hlsearch
set incsearch

set ignorecase
set smartcase

"-------------------------------------------------------------------------------
" splitting
"-------------------------------------------------------------------------------

set splitbelow
set splitright

"==============================================================================="
"                                                                               "
" look and feel                                                                 "
"                                                                               "
"==============================================================================="

"-------------------------------------------------------------------------------
" colors
"-------------------------------------------------------------------------------

set t_Co=256

set background=dark

"let g:solarized_bold = 1
"let g:solarized_contrast = 'normal'
"let g:solarized_italic = 1

"colorscheme iceberg
"colorscheme tir_black
"colorscheme jellyx
"colorscheme sebocean
"colorscheme solarized
"colorscheme summerfruit256
"colorscheme summerfruit_tap
"colorscheme summerfruit_tap
"colorscheme ir_black
colorscheme github

"-------------------------------------------------------------------------------
" font
"-------------------------------------------------------------------------------

"set guifont=Andale\ Mono:h14
"set guifont=PT\ Mono:h14
"set guifont=Anonymice\ Powerline:h15
"set guifont=Menlo\ for\ Powerline:h14
"set guifont=Fira\ Mono\ For\ Powerline:h14
"set guifont=MonacoB2:h13
"set guifont=MonacoB\ for\ Powerline:h13
"set guifont=Inconsolata\ LGC:h14
"set guifont=Input\ Mono\ Narrow:h14
"set guifont=Source\ Code\ Pro\ For\ Powerline:h15

if hostname() == 'Alexeys-MacBook-Pro.local'
  "set linespace=2
  "set guifont=Andale\ Mono\ Mt\ Std:h14

  "set linespace=3
  "set guifont=Anonymice\ Powerline:h15

  set linespace=-4
  set guifont=Inconsolata\ LGC:h14
else
  set linespace=-3
  set guifont=Inconsolata\ LGC:h14
endif

"-------------------------------------------------------------------------------
" indicators
"-------------------------------------------------------------------------------

" highlight current line (might slow down navigation)
set cursorline
" remove left-hand scroll bar
set guioptions-=L
" remove right-hand scroll bar
set guioptions-=r
" show line numbering
set number
" hide mode indicator
set noshowmode
" show number of lines or characters selected in right bottom corner
set showcmd

"-------------------------------------------------------------------------------
" startup
"-------------------------------------------------------------------------------

set shortmess+=I

"-------------------------------------------------------------------------------
" transparency
"-------------------------------------------------------------------------------

set transparency=0

"================================================================================
"                                                                               =
" abbreviations                                                                 =
"                                                                               =
"================================================================================

" TODO

"================================================================================
"                                                                               =
" common autocommands                                                           =
"                                                                               =
" exclamation mark means to remove all autocommands associated with event and   =
" pattern and a new command - the point here is that by default autocommands    =
" are accumulated every time .vimrc is sourced.                                 =
"                                                                               =
"================================================================================

"autocmd! BufRead * call s:SetBufhidden()

augroup reload
  autocmd!
  autocmd BufWritePost .vimrc source %
augroup END

let s:prevtabnr = tabpagenr()
let s:prevtabcount = tabpagenr('$')
augroup tabs
  autocmd!
  autocmd TabEnter * call s:GoToPrevTab()
augroup END

" TODO
"augroup large_files
  "autocmd!
  "autocmd FileType yaml call s:PrepareLargeFile(expand('<afile>'))
"augroup END

"================================================================================
"                                                                               =
" common maps                                                                   =
"                                                                               =
" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)             =
"                                                                               =
" NOTE: don't use noremap for plugin mappings                                   =
"================================================================================

" Leader: global and plugin mappings
" LocalLeader: mappings local to current buffer

let mapleader = ','
let maplocalleader = '\'

"================================================================================
" normal mode                                                                   =
"================================================================================

"-------------------------------------------------------------------------------
" copy current file name to clipboard
" (relative to PWD or absolute path if file is not in current dir)
"-------------------------------------------------------------------------------

" :help expand
nnoremap <silent> <Leader>yf :let @*=expand('%')<CR>

"-------------------------------------------------------------------------------
" editing without leaving normal mode
"-------------------------------------------------------------------------------

" delete trailing whitespaces
nnoremap <silent> <Leader>dt :%s/\s\+$//<CR>:w<CR>:nohlsearch<CR>
" insert newline after current line
nmap <silent> <CR> o<Esc>
" insert newline before current line
nnoremap <silent> <S-CR> O<Esc>
" insert space
nnoremap <silent> <Space> i<Space><Esc>l

"-------------------------------------------------------------------------------
" editing popular files
"-------------------------------------------------------------------------------

nnoremap <Leader>ec :tabnew<CR>:edit ~/.vim/colors/summerfruit_tap.vim<CR>
nnoremap <Leader>ev :tabnew<CR>:edit $MYVIMRC<CR>
nnoremap <Leader>ez :tabnew<CR>:edit ~/.zshrc<CR>

"-------------------------------------------------------------------------------
" fullscreen
"-------------------------------------------------------------------------------

" doesn't work this way - change toggle fullscreen mode key in
" System Preferences -> Keyboard -> Shortcuts -> App Shortcuts
"macmenu Window.Toggle\ Full\ Screen\ Mode key=<nop>

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

nnoremap <PageUp> <C-u>
nnoremap <PageDown> <C-d>

nnoremap <C-j> 10j
nnoremap <C-k> 10k

nnoremap H ^
nnoremap L g_

" switch to last active tab
let g:lasttabnr = 1
nnoremap <silent> <C-Tab> :exec 'tabnext ' . g:lasttabnr<CR>
autocmd! TabLeave * let g:lasttabnr = tabpagenr()

"------- buffer ----------------------------------------------------------------

" same as <C-6>
"nnoremap <silent> <C-Tab> :b#<CR>

"------- mark history ----------------------------------------------------------

" this mapping must come before remapping <Tab>
nnoremap <C-g> <C-i>

"------- tab --------------------------------------------------------------------

" the same as using gT and gt
nnoremap <C-h> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>

"nnoremap <Left> :tabprevious<CR>
"nnoremap <Right> :tabnext<CR>
"nnoremap <Down> :tabfirst<CR>
"nnoremap <Up> :tablast<CR>

nnoremap <silent> <S-Left> :tabmove -1<CR>
nnoremap <silent> <S-Right> :tabmove +1<CR>
nnoremap <silent> <S-Down> :tabmove 0<CR>
nnoremap <silent> <S-Up> :tabmove<CR>

"-------------------------------------------------------------------------------
" reload file using different encoding
"-------------------------------------------------------------------------------

nnoremap <Leader>cw :edit ++encoding=cp1251<CR>
nnoremap <Leader>cu :edit ++encoding=utf-8<CR>

"-------------------------------------------------------------------------------
" save
"-------------------------------------------------------------------------------

nnoremap <Tab> :w<CR>

"-------------------------------------------------------------------------------
" search
"-------------------------------------------------------------------------------

" turn off highlighting and clear messages
nnoremap <silent> <Backspace> :nohlsearch<Bar>:echo<CR>
nnoremap <silent> <C-c> <C-c>:nohlsearch<Bar>:echo<CR>

"-------------------------------------------------------------------------------
" sourcing configuration files
"-------------------------------------------------------------------------------

nnoremap <Leader>.v :source $MYVIMRC<CR>

"-------------------------------------------------------------------------------
" suppress unwanted keys
"-------------------------------------------------------------------------------

nnoremap Q <nop>

"-------------------------------------------------------------------------------
" yanking
"-------------------------------------------------------------------------------

nnoremap Y y$

"================================================================================
" insert mode                                                                   =
"================================================================================

"inoremap <Esc> <nop>

"-------------------------------------------------------------------------------
" editing
"-------------------------------------------------------------------------------

inoremap <C-d> <Delete>

"-------------------------------------------------------------------------------
" generation of complementary brackets
"-------------------------------------------------------------------------------

inoremap {{ {<Space><Space>}<Esc>hi
inoremap (( ()<Esc>i
"inoremap [[ []<Esc>i

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

inoremap <PageUp> <Esc><C-u>i
inoremap <PageDown> <Esc><C-d>i

inoremap <C-h> <Left>
inoremap <C-l> <Right>
inoremap <C-j> <Down>
inoremap <C-k> <Up>

"------- tab --------------------------------------------------------------------

inoremap <silent> <A-S-Left> <C-o>:tabmove -1<CR>
inoremap <silent> <A-S-Right> <C-o>:tabmove +1<CR>
inoremap <silent> <A-S-Down> <C-o>:tabmove 0<CR>
inoremap <silent> <A-S-Up> <C-o>:tabmove<CR>

"================================================================================
" visual mode                                                                   =
"================================================================================

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

vnoremap <C-j> 10j
vnoremap <C-k> 10k

vnoremap H ^
vnoremap L $

"-------------------------------------------------------------------------------
" shifting
"-------------------------------------------------------------------------------

vnoremap < <gv
vnoremap > >gv

"-------------------------------------------------------------------------------
" searching
"-------------------------------------------------------------------------------

vnorem // y/<C-r>"<CR>

"================================================================================
"                                                                               =
" load macros                                                                   =
"                                                                               =
"================================================================================

runtime macros/matchit.vim

"================================================================================
"                                                                               =
" plugin settings                                                               =
"                                                                               =
"================================================================================

"-------------------------------------------------------------------------------
" ag.vim
"-------------------------------------------------------------------------------

let g:agprg="ag --vimgrep --nocolor --ignore-dir={log,public,tmp,spec/vcr_cassettes}"
map <Leader>/ :Ag<Space>

"-------------------------------------------------------------------------------
" CamelCaseMotion
"-------------------------------------------------------------------------------

map <silent> w <Plug>CamelCaseMotion_w
map <silent> e <Plug>CamelCaseMotion_e
map <silent> b <Plug>CamelCaseMotion_b

"-------------------------------------------------------------------------------
" command-t
"-------------------------------------------------------------------------------

let g:CommandTMaxHeight = 17
let g:CommandTMaxFiles = 25000

nmap <F1> :CommandT<CR>
nmap <S-F1> :CommandTBuffer<CR>
nmap <Leader><F1>r :CommandTFlush<CR>:CommandT<CR>

" http://www.adp-gmbh.ch/vim/user_commands.html
" http://vimdoc.sourceforge.net/htmldoc/usr_40.html
"command! -nargs=+ GotoOrOpen call GotoOrOpen("<args>")
"command! -nargs=+ GotoOrOpenTab call GotoOrOpenTab("<args>")

"let g:CommandTAcceptSelectionCommand = 'GotoOrOpen'
"let g:CommandTAcceptSelectionTabCommand = 'GotoOrOpenTab'

"-------------------------------------------------------------------------------
" nerdcommenter
"
" <C-1>,<C-2>,etc. are not allowed as well as <C-/>. see for details:
" http://vim.1045645.n5.nabble.com/mapping-control-0-1-or-backtick-td1189910.html
"-------------------------------------------------------------------------------

map <Leader><Space> <Plug>NERDCommenterToggle

"-------------------------------------------------------------------------------
" LustyExplorer
"-------------------------------------------------------------------------------

nmap <F3> :LustyBufferGrep<CR>
nmap <Leader>l :LustyBufferGrep<CR>

silent! unmap <Leader>lf
silent! unmap <Leader>lr
silent! unmap <Leader>lb
silent! unmap <Leader>lg

"-------------------------------------------------------------------------------
" nerdtree
"-------------------------------------------------------------------------------

nmap <F2> :NERDTreeToggle<CR>
nmap <Leader><F2>r :NERDTree<CR>
nmap <Leader><F2>f :NERDTreeFind<CR>

"-------------------------------------------------------------------------------
" rspec.vim
"-------------------------------------------------------------------------------

autocmd! BufNewFile,BufRead *_spec.rb set filetype=rspec

"-------------------------------------------------------------------------------
" Specky
"-------------------------------------------------------------------------------

let g:speckySpecSwitcherKey = '<F4>'

nmap <Leader><F4> <C-w><C-v><C-w>l<F4>

"-------------------------------------------------------------------------------
" supertab
"-------------------------------------------------------------------------------

let g:SuperTabDefaultCompletionType = '<C-n>'
let g:SuperTabCrMapping = 1

"-------------------------------------------------------------------------------
" vim-airline
" http://joshldavis.com/2014/04/05/vim-tab-madness-buffers-vs-tabs
"-------------------------------------------------------------------------------

" always show airline
set laststatus=2

"let g:airline#extensions#tabline#enabled = 1
"let g:airline#extensions#tabline#fnamemod = ':t'
"let g:airline#extensions#tabline#left_sep = '⮀'
"let g:airline#extensions#tabline#left_alt_sep = '⮁'

"let g:airline_theme = 'molokai'
"let g:airline_theme = 'hybrid'
"let g:airline_theme = 'lucius'
"let g:airline_theme = 'tomorrow'
"let g:airline_theme = 'powerlineish'
"let g:airline_theme = 'dark'
"let g:airline_theme = 'light'
let g:airline_theme = 'base16'

let g:airline_powerline_fonts = 0

let g:airline_left_sep = '⮀'
let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
let g:airline_right_alt_sep = '⮃'

" https://github.com/bling/vim-airline/issues/193
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
end

let g:airline_symbols.branch = '⭠'
let g:airline_symbols.readonly = '⭤'
let g:airline_symbols.linenr = '⭡'

"-------------------------------------------------------------------------------
" vim-buffergator
"-------------------------------------------------------------------------------

"let g:buffergator_sort_regime = 'basename'
"let g:buffergator_split_size = 20
"let g:buffergator_suppress_keymaps = 1
"let g:buffergator_viewport_split_policy = 'R'
"let g:buffergator_vsplit_size = 60

"nmap <F3>b :BuffergatorOpen<CR>
"nmap <F3>t :BuffergatorTabsOpen<CR>
nmap <silent> <C-p> :BuffergatorMruCyclePrev<CR>
nmap <silent> <C-n> :BuffergatorMruCycleNext<CR>

"-------------------------------------------------------------------------------
" vim-fugitive
"-------------------------------------------------------------------------------

nmap <F6> :Gvdiff<CR>

"-------------------------------------------------------------------------------
" vim-session
"-------------------------------------------------------------------------------

let g:session_autoload = 'no'
let g:session_autosave = 'no'

nmap <F7>d :DeleteSession<Space>
nmap <F7>o :OpenSession<Space>
nmap <F7>s :SaveSession<Space>

"================================================================================
"                                                                               =
" functions                                                                     =
"                                                                               =
"================================================================================

" s: scoping prefix means that function is scoped to current script file;
" don't use for every buffer since it will prevent from using buffer history
function! s:SetBufhidden()
  " most explorer plugins have buftype=nofile
  " while normal buffers have buftype=<empty>;
  " using & means that I refer to option -
  " not variable that might have the same name
  if empty(&buftype)
    " wipe buffer when it's no longer displayed in any window
    " (overrides behaviour set with global hidden option) -
    " setting this option to delete still makes command-t
    " open deleted buffers in the same tab
    " (because information about them is still available
    " even though buffers are not listed in buffer list)
    setlocal bufhidden=wipe
  endif
endfunction

" http://stackoverflow.com/questions/14079149
function! s:GoToPrevTab()
  if tabpagenr('$') < s:prevtabcount && tabpagenr() > 1 && tabpagenr() == s:prevtabnr
    tabprevious
  endif

  let s:prevtabnr = tabpagenr()
  let s:prevtabcount = tabpagenr('$')
endfunction

" a:000: http://learnvimscriptthehardway.stevelosh.com/chapters/24.html;
" we use sbuffer because buffer function doesn't respect switchbuf option;
" we need to wipe out unloaded buffer because otherwise sbuffer will open
" it in a split window instead of a new tab

" CommandT documentation:
"   Note that if you have |'nohidden'| set and there are unsaved
"   changes in the current window when you press <CR> then opening in the current
"   window would fail; in this case Command-T will open the file in a new split.
" => this function is not even called if there're unsaved changes in current window
function! GotoOrOpen(...)
  for file in a:000
    if bufexists(bufnr(file)) && !bufloaded(bufnr(file))
      exec 'bwipeout ' . file
    endif

    if bufnr(file) != -1
      exec 'sbuffer ' . file
    else
      exec 'edit ' . file
    endif
  endfor
endfunction

function! GotoOrOpenTab(...)
  for file in a:000
    if bufexists(bufnr(file)) && !bufloaded(bufnr(file))
      exec 'bwipeout ' . file
    endif

    if bufnr(file) != -1
      exec 'sbuffer ' . file
    else
      exec 'tabedit ' . file
    endif
  endfor
endfunction

" TODO
" http://vim.wikia.com/wiki/Faster_loading_of_large_files
"function! s:PrepareLargeFile(fname)
  "echom fname
  "if getfsize(fname) > 1024 * 10
    "echom 'aoeu'
  "endif
"endfunction
