"*******************************************************************************
"                                                                              *
" NOTE: these keys generate the same characters:                               *
"       - <Tab> and <C-i>                                                      *
"       - <CR> and <C-m>                                                       *
"       - <Esc> and <C-[>                                                      *
"                                                                              *
" USEFUL RESOURCES:                                                            *
"                                                                              *
" http://www.ibm.com/developerworks/library/l-vim-script-1/                    *
"                                                                              *
"*******************************************************************************

"===============================================================================
"                                                                              =
" compatibility with morr's configuration                                      =
"                                                                              =
" http://www.arl.wustl.edu/~fredk/Courses/Docs/vim/options.html                =
" http://superuser.com/questions/224607                                        =
"                                                                              =
"===============================================================================

source ~/.dotfiles/.vimrc.morr.basic

"===============================================================================
"                                                                              =
" pathogen                                                                     =
"                                                                              =
" https://github.com/tpope/vim-pathogen                                        =
" http://logicalfriday.com/2011/07/18/using-vim-with-pathogen/                 =
" http://www.vim.org/scripts/script.php?script_id=2332                         =
"                                                                              =
"===============================================================================

filetype off
execute pathogen#infect()
filetype plugin indent on

syntax on

"===============================================================================
"                                                                              =
" performance tweaks                                                           =
"                                                                              =
" http://stackoverflow.com/questions/16902317                                  =
"                                                                              =
"===============================================================================

set synmaxcol=200
set regexpengine=2

"===============================================================================
"                                                                              =
" general settings                                                             =
"                                                                              =
"===============================================================================

set nocompatible
set nowrap

"-------------------------------------------------------------------------------
" editing
"-------------------------------------------------------------------------------

set cpoptions+=$
set colorcolumn=81
"execute 'set colorcolumn=' . join(range(81,250), ',')

"-------------------------------------------------------------------------------
" backup and swap files
"-------------------------------------------------------------------------------

set backup
set noswapfile

" swap files (currently disabled)
" http://vim.wikia.com/wiki/Editing_crontab
"set directory=~/.vim/tmp//
"set backupskip+=/private/tmp/*

"-------------------------------------------------------------------------------
" buffers
"-------------------------------------------------------------------------------

" http://vim.wikia.com/wiki/Using_tab_pages
set switchbuf=usetab,newtab
" http://usevim.com/2012/10/19/vim101-set-hidden
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

set scrolloff=8

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

"=============================================================================="
"                                                                              "
" look and feel                                                                "
"                                                                              "
"=============================================================================="

"-------------------------------------------------------------------------------
" colors
"-------------------------------------------------------------------------------

set t_Co=256

set background=light
set transparency=0

let g:solarized_bold = 0
let g:solarized_contrast = 'normal'
let g:solarized_diffmode = 'normal'
let g:solarized_italic = 0
let g:solarized_underline = 1

"colorscheme summerfruit_tap
"colorscheme ir_black_tap
"colorscheme ir_black_morr
"colorscheme sebocean_dasha
"colorscheme github
"colorscheme vylight
"colorscheme scheakur

"colorscheme solarized
colorscheme PaperColor

"-------------------------------------------------------------------------------
" font
"-------------------------------------------------------------------------------

"set linespace=-1
"set guifont=Inconsolata-dz\ For\ Powerline:h14

"set linespace=-2
"set guifont=Inconsolata\ LGC:h14

"set linespace=2
"set guifont=Input\ Mono\ Narrow:h13

"set linespace=3
"set guifont=Andale\ Mono:h14

"set linespace=2
"set guifont=Andale\ Mono\ MT\ Std:h14

"set linespace=4
"set guifont=Cousine:h14

if hostname() == 'MacBook-Pro.local'
  set linespace=5
  set guifont=Andale\ Mono\ MT\ Std:h14
  "set linespace=5
  "set guifont=Andale\ Mono:h14
  "set linespace=5
  "set guifont=MonacoB\ for\ Powerline:h13
  "set linespace=1
  "set guifont=Inconsolata\ LGC\ for\ Powerline:h14
else
  "set linespace=3
  "set guifont=MonacoB\ for\ Powerline:h13
  set linespace=4
  set guifont=Andale\ Mono\ MT\ Std:h14
  "set linespace=4
  "set guifont=Andale\ Mono:h14
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
" hide mode indicator
set noshowmode
" show number of lines or characters selected in right bottom corner
set showcmd
" disable some gui popups
set guioptions+=c
" this is the main cause of sluggish scrolling!
" set after colorscheme - or else it can redefined inside it
set nocursorline

" show line numbering
if hostname() == 'MacBook-Pro.local'
  set nonumber
else
  set number
endif

"-------------------------------------------------------------------------------
" startup
"-------------------------------------------------------------------------------

set shortmess+=I

" https://rvm.io/integration/vim
" https://github.com/scrooloose/syntastic/issues/1407
set shell=/bin/sh

" maximize on startup
" NOTE: it maximizes the 1st column when using vimdiff!
"set lines=999
"set columns=999

"===============================================================================
"                                                                              =
" abbreviations                                                                =
"                                                                              =
"===============================================================================

iabbrev ii i18n_i
iabbrev tt i18n_t
iabbrev teh the

"===============================================================================
"                                                                              =
" common autocommands                                                          =
"                                                                              =
" exclamation mark means to remove all autocommands associated with event,     =
" pattern and command - the point here is that by default autocommands are     =
" are accumulated every time .vimrc is sourced.                                =
"                                                                              =
"===============================================================================

augroup vimrc
  autocmd!
  autocmd BufWritePost .vimrc source %
augroup END

let s:prevtabnr = tabpagenr()
let s:prevtabcount = tabpagenr('$')
augroup tabs
  autocmd!
  autocmd TabEnter * call s:GoToPrevTab()
augroup END

augroup filetypes
  autocmd!
  autocmd BufRead,BufNewFile *.arb set filetype=ruby
augroup END

augroup quickfix
  autocmd!
  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
augroup END

augroup diffmode
  autocmd!
  autocmd FilterWritePre * call SetDiffMode()
augroup END

augroup backup
  autocmd!
  autocmd BufWritePre * call SetBackupDir()
augroup END

"===============================================================================
"                                                                              =
" common maps                                                                  =
"                                                                              =
" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)            =
"                                                                              =
" NOTE: don't use noremap for plugin mappings                                  =
"                                                                              =
"===============================================================================

" Leader: global and plugin mappings
" LocalLeader: mappings local to current buffer

let mapleader = ','
let maplocalleader = '\'

"===============================================================================
" normal mode                                                                  =
"===============================================================================

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

nnoremap <Leader>ev :tabnew<CR>:edit $MYVIMRC<CR>
nnoremap <Leader>ec :tabnew<CR>:edit ~/.vim/colors/summerfruit_tap.vim<CR>
nnoremap <Leader>ez :tabnew<CR>:edit ~/.zshrc<CR>
nnoremap <Leader>eu :tabnew<CR>:edit ~/.vim/update_bundles<CR>

"-------------------------------------------------------------------------------
" fullscreen
"-------------------------------------------------------------------------------

" doesn't work this way - change toggle fullscreen mode key in
" System Preferences -> Keyboard -> Shortcuts -> App Shortcuts
"macmenu Window.Toggle\ Full\ Screen\ Mode key=<nop>

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

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
nmap <C-h> :tabprevious<CR>
nmap <C-l> :tabnext<CR>

nmap ˙ :tabmove -1<CR>
nmap ¬ :tabmove +1<CR>

"------- window ----------------------------------------------------------------

" http://vim.wikia.com/wiki/Open_file_under_cursor
"set isfname-=.
"nnoremap <C-w>F :vertical wincmd f<CR>
nmap <C-w>F <C-w>f<C-w>L

nmap <silent> <S-Up> :resize +5<CR>
nmap <silent> <S-Down> :resize -5<CR>
nmap <silent> <S-Left> :vertical resize -5<CR>
nmap <silent> <S-Right> :vertical resize +5<CR>

"-------------------------------------------------------------------------------
" reload file using different encoding
"-------------------------------------------------------------------------------

nnoremap <Leader>cw :edit ++encoding=cp1251<CR>
nnoremap <Leader>cu :edit ++encoding=utf-8<CR>

"-------------------------------------------------------------------------------
" save
"-------------------------------------------------------------------------------

nnoremap <silent> <Tab> :w<CR>

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

"-------------------------------------------------------------------------------
" useful maps
"-------------------------------------------------------------------------------

" TODO use function to toggle focus

"===============================================================================
" insert mode                                                                  =
"===============================================================================

"inoremap <Esc> <nop>
inoremap <C-c> <Esc>

"-------------------------------------------------------------------------------
" editing
"-------------------------------------------------------------------------------

inoremap <C-d> <Delete>

"-------------------------------------------------------------------------------
" generation of complementary brackets
"-------------------------------------------------------------------------------

inoremap {{ {<Space><Space>}<Esc>hi
inoremap }} {}<Esc>i
inoremap )) ()<Esc>i
"inoremap [[ []<Esc>i

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

inoremap <C-b> <Left>
inoremap <C-f> <Right>
inoremap <C-n> <Down>
inoremap <C-p> <Up>
inoremap <C-a> <C-o>I
inoremap <C-e> <C-o>A

"------- tab --------------------------------------------------------------------

inoremap <silent> <A-S-Left> <C-o>:tabmove -1<CR>
inoremap <silent> <A-S-Right> <C-o>:tabmove +1<CR>
inoremap <silent> <A-S-Down> <C-o>:tabmove 0<CR>
inoremap <silent> <A-S-Up> <C-o>:tabmove<CR>

"===============================================================================
" visual mode                                                                  =
"===============================================================================

"-------------------------------------------------------------------------------
" editing
" http://stackoverflow.com/a/10723838/3632318
"-------------------------------------------------------------------------------

"vnoremap p "_dP

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

vnoremap <C-j> 10j
vnoremap <C-k> 10k

vnoremap H ^
vnoremap L g_

"-------------------------------------------------------------------------------
" shifting
"-------------------------------------------------------------------------------

vnoremap < <gv
vnoremap > >gv

"-------------------------------------------------------------------------------
" searching
"-------------------------------------------------------------------------------

vnorem * y/<C-r>"<CR>

"===============================================================================
"                                                                              =
" load macros                                                                  =
"                                                                              =
"===============================================================================

runtime macros/matchit.vim

"===============================================================================
"                                                                              =
" plugin settings                                                              =
"                                                                              =
"===============================================================================

"-------------------------------------------------------------------------------
" ag.vim
"-------------------------------------------------------------------------------

let g:ag_prg = 'ag --vimgrep --column -Q --ignore-dir={log,public,tmp,spec/vcr_cassettes}'
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

"let g:CommandTCancelMap = '<F1>'
let g:CommandTMatchWindowAtTop = 0
let g:CommandTMatchWindowReverse = 0
let g:CommandTMaxHeight = 17
let g:CommandTMaxFiles = 25000
let g:CommandTWildIgnore = &wildignore . 'public/images/**,tmp/**,public/assets/**,**/*.log'

nmap <F1> :CommandT<CR>
imap <F1> <Esc>:CommandT<CR>
nmap <S-F1> :CommandTBuffer<CR>
nmap <Leader><F1>r :CommandTFlush<CR>:CommandT<CR>

" http://vimdoc.sourceforge.net/htmldoc/usr_40.html
"command! -nargs=+ GotoOrOpen call GotoOrOpen('<args>')
command! -nargs=+ GotoOrOpenTab call GotoOrOpenTab('<args>')

"let g:CommandTAcceptSelectionCommand = 'GotoOrOpen'
let g:CommandTAcceptSelectionTabCommand = 'GotoOrOpenTab'

"-------------------------------------------------------------------------------
" lightline.vim
"-------------------------------------------------------------------------------

"set laststatus=2
"let g:lightline = { 'colorscheme': 'solarized' }

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
nmap <Leader><F2>f :NERDTreeFind<CR>

"-------------------------------------------------------------------------------
" rspec.vim
"-------------------------------------------------------------------------------

"autocmd! BufNewFile,BufRead *_spec.rb set filetype=rspec

"-------------------------------------------------------------------------------
" Specky
"-------------------------------------------------------------------------------

"let g:speckySpecSwitcherKey = '<F4>'

"nmap <Leader><F4> <C-w><C-v><C-w>l<F4>

"-------------------------------------------------------------------------------
" supertab
"-------------------------------------------------------------------------------

let g:SuperTabDefaultCompletionType = '<C-n>'
let g:SuperTabCrMapping = 1

"-------------------------------------------------------------------------------
" syntastic
"-------------------------------------------------------------------------------

let g:syntastic_ruby_mri_exec = '~/.rvm/rubies/ruby-2.3.0/bin/ruby'

" http://vim.wikia.com/wiki/Simplifying_regular_expressions_using_magic_and_no-magic
"
" '\m^shadowing outer local variable'
let g:syntastic_ruby_mri_quiet_messages = {
\   'regex': [
\     '\m`&'' interpreted as argument prefix',
\     '\m`*'' interpreted as argument prefix',
\     '\mambiguous first argument; put parentheses or a space even after `/'' operator'
\   ]
\ }

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

" dark
"let g:airline_theme = 'molokai'
"let g:airline_theme = 'powerlineish'
"let g:airline_theme = 'dark'
"let g:airline_theme = 'serene'
"let g:airline_theme = 'ubaryd'
"let g:airline_theme = 'laederon'

" light
"let g:airline_theme = 'hybrid'
let g:airline_theme = 'lucius'
"let g:airline_theme = 'tomorrow'
"let g:airline_theme = 'light'
"let g:airline_theme = 'base16'
"let g:airline_theme = 'zenburn'
"let g:airline_theme = 'solarized'
"let g:airline_theme = 'papercolor'

let g:airline_powerline_fonts = 0

" https://github.com/vim-airline/vim-airline/blob/master/doc/airline.txt
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

let g:airline_left_sep = '⮀'
let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
let g:airline_right_alt_sep = '⮃'
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

nmap <F3> :BuffergatorToggle<CR>
nmap <silent> <C-p> :BuffergatorMruCyclePrev<CR>
nmap <silent> <C-n> :BuffergatorMruCycleNext<CR>

"-------------------------------------------------------------------------------
" vim-easymotion
"-------------------------------------------------------------------------------

let g:EasyMotion_enter_jump_first = 1

nmap <Leader>s <Plug>(easymotion-s2)
nmap <Leader>f <Plug>(easymotion-f)
nmap <Leader>F <Plug>(easymotion-F)
nmap <Leader>w <Plug>(easymotion-bd-w)

"-------------------------------------------------------------------------------
" vim-fugitive
"-------------------------------------------------------------------------------

nmap <F6> :Gvdiff<CR>

"-------------------------------------------------------------------------------
" vim-gitgutter
"-------------------------------------------------------------------------------

set updatetime=500

let g:gitgutter_enabled = 0
"let g:gitgutter_map_keys = 0
let g:gitgutter_highlight_lines = 1

nmap <silent> <Leader>ht :GitGutterToggle<CR>

"nmap ]c <Plug>GitGutterNextHunk
"nmap [c <Plug>GitGutterPrevHunk

"nmap <Leader>hp <Plug>GitGutterPreviewHunk
"nmap <Leader>hr <Plug>GitGutterRevertHunk
"nmap <Leader>hs <Plug>GitGutterStageHunk

"-------------------------------------------------------------------------------
" vim-hugefile
"-------------------------------------------------------------------------------

let g:hugefile_trigger_size = 0.1

"-------------------------------------------------------------------------------
" vim-markdown
"-------------------------------------------------------------------------------

let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_frontmatter = 1

"-------------------------------------------------------------------------------
" vim-rails
"
" example projections: https://gist.github.com/henrik/5676109
"-------------------------------------------------------------------------------

nmap <F4> :A<CR>
nmap <Leader><F4> :AV<CR>

let g:rails_projections = {
\   'app/admin/*.rb': {
\     'alternate': 'spec/controllers/admin/{}_controller_spec.rb'
\   },
\   'spec/controllers/admin/*_controller_spec.rb': {
\     'alternate': 'app/admin/{}.rb'
\   },
\ }

"-------------------------------------------------------------------------------
" vim-session
"-------------------------------------------------------------------------------

let g:session_autoload = 'no'
let g:session_autosave = 'no'

nmap <F7>d :DeleteSession<Space>
nmap <F7>o :OpenSession<Space>
nmap <F7>s :SaveSession<Space>

"===============================================================================
"                                                                              =
" functions                                                                    =
"                                                                              =
"===============================================================================

" http://stackoverflow.com/questions/14079149
function! s:GoToPrevTab()
  if tabpagenr('$') < s:prevtabcount && tabpagenr() > 1 && tabpagenr() == s:prevtabnr
    tabprevious
  endif

  let s:prevtabnr = tabpagenr()
  let s:prevtabcount = tabpagenr('$')
endfunction

" returns 1 (true) if buffer hidden or 0 (false) otherwise
" http://stackoverflow.com/a/8459043
function! BufHidden(buf)
  let active_buffers = []
  let tabs = range(1, tabpagenr('$'))

  call map(tabs, 'extend(active_buffers, tabpagebuflist(v:val))')
  return (bufexists(a:buf) && index(active_buffers, a:buf) == -1)
endfunction

" a:000: http://learnvimscriptthehardway.stevelosh.com/chapters/24.html
" sbuffer: `buffer` function doesn't respect `switchbuf` option
function! GotoOrOpenTab(...)
  for file in a:000
    " if buffer exists and not hidden
    if bufexists(file) && !BufHidden(bufnr(file))
      exec 'sbuffer ' . file
    else
      exec 'tabedit ' . file
    endif
  endfor
endfunction

" settings for difftool and mergetool
function! SetDiffMode()
  if &diff
    set background=dark
    colorscheme solarized

    " always show tabbar
    set showtabline=2
    " don't show airline
    set laststatus=0
  endif
endfunction

function! SetBackupDir()
  let l:backupdir = $HOME . '/.vim/backup' . expand('%:p:h')
  if !isdirectory(l:backupdir)
    call mkdir(l:backupdir, 'p', 0700)
  endif

  let &backupdir = l:backupdir
  let &backupext = strftime('~(%Y-%m-%d %H:%M:%S)')
endfunction
