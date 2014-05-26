"********************************************************************************
"                                                                               *
" Note: These keys generate the same characters:                                *
"       - <Tab> and <C-i>                                                       *
"       - <CR> and <C-m>                                                        *
"       - <Esc> and <C-[>                                                       *
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
" completion menu
"-------------------------------------------------------------------------------

set wildmenu
set wildmode=longest:full,full

"-------------------------------------------------------------------------------
" indentation
"
" setting smartindent might lead to incorrect comment indentation on new line
"-------------------------------------------------------------------------------

set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2

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

"colorscheme solarized
"colorscheme gravity
colorscheme summerfruit_tap
"colorscheme summerfruit256
"colorscheme tap
"colorscheme iceberg
"colorscheme ironman
"colorscheme buttercream
"colorscheme sebocean
"colorscheme ir_black
"colorscheme jellyx
"colorscheme sift
"colorscheme zenesque
"colorscheme whitedust

"-------------------------------------------------------------------------------
" font
"-------------------------------------------------------------------------------

"set guifont=Menlo:h14
"set guifont=Andale\ Mono:h15
set guifont=MonacoB2:h13
"set guifont=MonacoB\ for\ Powerline:h13
"set guifont=Anonymice\ Powerline:h16

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

"-------------------------------------------------------------------------------
" startup
"-------------------------------------------------------------------------------

set shortmess+=I
cd ~/dev/uptimus


"================================================================================
"                                                                               =
" common autocommands                                                           =
"                                                                               =
" exclamation mark means to remove all autocommands associated with event and   =
" pattern and a new command - the point here is that by default autocommands    =
" are accumulated every time .vimrc is sourced.                                 =
"                                                                               =
"================================================================================

autocmd! BufWritePost .vimrc source %


"================================================================================
"                                                                               =
" common maps                                                                   =
"                                                                               =
" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)             =
"                                                                               =
"================================================================================

let mapleader = ','
let maplocalleader = '\'


"================================================================================
" normal mode                                                                   =
"================================================================================

"-------------------------------------------------------------------------------
" editing without leaving normal mode
"-------------------------------------------------------------------------------

" delete trailing whitespaces
nmap <silent> <Leader>dt :%s/\s\+$//<CR>:w<CR>:nohlsearch<CR>
" insert newline after current line
nmap <silent> <CR> o<Esc>
" insert newline before current line
nmap <silent> <S-CR> O<Esc>
" insert space
nmap <silent> <Space> i<Space><Esc>l

"-------------------------------------------------------------------------------
" editing popular files
"-------------------------------------------------------------------------------

nmap <LocalLeader>ec :edit ~/.vim/colors/summerfruit_tap.vim<CR>
nmap <LocalLeader>ev :edit $MYVIMRC<CR>
nmap <LocalLeader>ez :edit ~/.zshrc<CR>

"-------------------------------------------------------------------------------
" fullscreen
"-------------------------------------------------------------------------------

" doesn't work this way - change toggle fullscreen mode key in
" System Preferences -> Keyboard -> Shortcuts -> App Shortcuts
"macmenu Window.Toggle\ Full\ Screen\ Mode key=<nop>

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

nmap <PageUp> <C-u>
nmap <PageDown> <C-d>

nmap <C-j> 10j
nmap <C-k> 10k

"------- buffer ----------------------------------------------------------------

" same as <C-6>
nmap <silent> <C-Tab> :b#<CR>

"------- tab --------------------------------------------------------------------

"nmap <C-h> gT
"nmap <C-l> gt

nmap <C-h> :tabprevious<CR>
nmap <C-l> :tabnext<CR>

nmap <A-Left> :tabprevious<CR>
nmap <A-Right> :tabnext<CR>
nmap <A-Down> :tabfirst<CR>
nmap <A-Up> :tablast<CR>

nmap <silent> <A-S-Left> :tabmove -1<CR>
nmap <silent> <A-S-Right> :tabmove +1<CR>
nmap <silent> <A-S-Down> :tabmove 0<CR>
nmap <silent> <A-S-Up> :tabmove<CR>

" the same for MacVim only (doesn't work though):
"macmenu Window.Select\ Previous\ Tab key=<C-h>
"macmenu Window.Select\ Next\ Tab key=<C-l>

"------- window -----------------------------------------------------------------

"nmap <silent> <M-h> :wincmd h<CR>
"nmap <silent> <M-j> :wincmd j<CR>
"nmap <silent> <M-k> :wincmd k<CR>
"nmap <silent> <M-l> :wincmd l<CR>

"-------------------------------------------------------------------------------
" reload file using different encoding
"-------------------------------------------------------------------------------

nmap <LocalLeader>cw :edit ++encoding=cp1251<CR>
nmap <LocalLeader>cu :edit ++encoding=utf-8<CR>

"-------------------------------------------------------------------------------
" search
"-------------------------------------------------------------------------------

" turn off highlighting and clear messages
nmap <silent> <Backspace> :nohlsearch<Bar>:echo<CR>
nnoremap <silent> <C-c> <C-c>:nohlsearch<Bar>:echo<CR>

"-------------------------------------------------------------------------------
" sourcing configuration files
"-------------------------------------------------------------------------------

nmap <Leader>.v :source $MYVIMRC<CR>

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

"-------------------------------------------------------------------------------
" editing
"-------------------------------------------------------------------------------

imap <C-d> <Delete>

"-------------------------------------------------------------------------------
" generation of complementary brackets
"-------------------------------------------------------------------------------

imap {{ {<Space><Space>}<Esc>hi
"imap (( ()<Esc>i
"imap [[ []<Esc>i

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

imap <PageUp> <Esc><C-u>i
imap <PageDown> <Esc><C-d>i

imap <C-h> <Left>
imap <C-l> <Right>
imap <C-j> <Down>
imap <C-k> <Up>

"------- tab --------------------------------------------------------------------

imap <silent> <A-S-Left> <C-o>:tabmove -1<CR>
imap <silent> <A-S-Right> <C-o>:tabmove +1<CR>
imap <silent> <A-S-Down> <C-o>:tabmove 0<CR>
imap <silent> <A-S-Up> <C-o>:tabmove<CR>


"================================================================================
" visual mode                                                                   =
"================================================================================

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

vmap <C-j> 10j
vmap <C-k> 10k

"-------------------------------------------------------------------------------
" shifting
"-------------------------------------------------------------------------------

vnoremap < <gv
vnoremap > >gv


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
nmap <Leader><F1>r :CommandTFlush<CR>:CommandT<CR>


"-------------------------------------------------------------------------------
" indentLine
"
" warning: this plugin disables syntax highlighting in slim files.
"-------------------------------------------------------------------------------

"let g:indentLine_char = '┆'
"let g:indentLine_color_gui = '#4C5B6B'


"-------------------------------------------------------------------------------
" nerdcommenter
"
" <C-1>,<C-2>,etc. are not allowed as well as <C-/>. see for details:
" http://vim.1045645.n5.nabble.com/mapping-control-0-1-or-backtick-td1189910.html
"-------------------------------------------------------------------------------

map <Leader><Space> <Plug>NERDCommenterToggle


"-------------------------------------------------------------------------------
" nerdtree
"-------------------------------------------------------------------------------

nmap <F2> :NERDTreeToggle<CR>
nmap <Leader><F2>r :NERDTree<CR>
nmap <Leader><F2>f :NERDTreeFind<CR>


"-------------------------------------------------------------------------------
" rspec.vim
"-------------------------------------------------------------------------------

au BufNewFile,BufRead *_spec.rb set filetype=rspec


"-------------------------------------------------------------------------------
" Specky
"-------------------------------------------------------------------------------

let g:speckySpecSwitcherKey = '<F4>'

nmap <Leader><F4> <C-w><C-v><C-w>l<F4>


"-------------------------------------------------------------------------------
" supertab
"-------------------------------------------------------------------------------

let g:SuperTabDefaultCompletionType = '<C-n>'


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
"let g:airline_theme = 'powerlineish'
"let g:airline_theme = 'dark'
let g:airline_theme = 'light'

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

let g:buffergator_sort_regime = 'basename'
let g:buffergator_split_size = 20
let g:buffergator_suppress_keymaps = 1
let g:buffergator_viewport_split_policy = 'R'
let g:buffergator_vsplit_size = 60

nmap <F3>b :BuffergatorOpen<CR>
nmap <F3>t :BuffergatorTabsOpen<CR>
nmap <silent> <C-p> :BuffergatorMruCyclePrev<CR>
nmap <silent> <C-n> :BuffergatorMruCycleNext<CR>


"-------------------------------------------------------------------------------
" vim-fugitive
"-------------------------------------------------------------------------------

nmap <F6> :Gdiff<CR>


"-------------------------------------------------------------------------------
" vim-session
"-------------------------------------------------------------------------------

let g:session_autoload = 'no'
let g:session_autosave = 'no'

nmap <F7>d :DeleteSession<Space>
nmap <F7>o :OpenSession<Space>
nmap <F7>s :SaveSession<Space>
