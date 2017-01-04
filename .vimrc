"*******************************************************************************
"                                                                              *
" NOTE: these keys generate the same characters:                               *
"       - <Tab> and <C-i>                                                      *
"       - <CR> and <C-m>                                                       *
"       - <Esc> and <C-[>                                                      *
"       - <C-S-h> and <C-h> (any character)                                    *
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
" vim-plug                                                                     =
"                                                                              =
" https://github.com/junegunn/vim-plug                                         =
"                                                                              =
"===============================================================================

call plug#begin('~/.vim/plugged')

" file types support

" it seemed to me that using vim-polyglot introduced some lag
" in general - when switching tabs, switching to visual mode, etc.
"Plug 'sheerun/vim-polyglot'
Plug 'elixir-lang/vim-elixir'
Plug 'kchmck/vim-coffee-script'
Plug 'keith/rspec.vim'
Plug 'tap349/vim-markdown'
Plug 'slim-template/vim-slim'
Plug 'vim-ruby/vim-ruby'

" ctrlp

Plug 'ctrlpvim/ctrlp.vim'
Plug 'jasoncodes/ctrlp-modified.vim'

" airline

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" git

Plug 'airblade/vim-gitgutter'
" https://github.com/c-brenn/phoenix.vim#installation
Plug 'tap349/vim-extradite' | Plug 'tpope/vim-fugitive'

" other

Plug 'ap/vim-css-color'
Plug 'bkad/CamelCaseMotion'
Plug 'c-brenn/phoenix.vim' | Plug 'tpope/vim-projectionist'
Plug 'easymotion/vim-easymotion'
Plug 'ervandew/supertab'
Plug 'flazz/vim-colorschemes'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'ludovicchabant/vim-gutentags'
Plug 'mhinz/vim-hugefile'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'tap349/ag.vim'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'xolox/vim-misc'
Plug 'yssl/QFEnter'

" unused (but still can be used again somewhen)

"Plug 'Yggdroot/indentLine'
"Plug 'itchyny/lightline.vim'
"Plug 'jamessan/vim-gnupg'
"Plug 'scheakur/vim-scheakur'
"Plug 'xolox/vim-session'

call plug#end()

"===============================================================================
"                                                                              =
" performance tweaks                                                           =
"                                                                              =
" http://stackoverflow.com/questions/16902317                                  =
"                                                                              =
"===============================================================================

set lazyredraw
set regexpengine=2
set synmaxcol=200

"===============================================================================
"                                                                              =
" general options                                                              =
"                                                                              =
"===============================================================================

syntax on

set nocompatible
set nospell
set nowrap

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

" http://usevim.com/2012/10/19/vim101-set-hidden
set hidden
" http://vim.wikia.com/wiki/Using_tab_pages
" `newtab` causes all quickfix files (Ag, etc.) to be opened in new tab
set switchbuf=usetab

"-------------------------------------------------------------------------------
" completion
"-------------------------------------------------------------------------------

" menu,preview - default
set completeopt+=longest

set wildmenu
set wildmode=longest:full,full
" used by command-t and ctrlp - it should be kept in sync with ~/.agignore
"
" don't prefix directories relative to project root with asterisk -
" that is prefer public/* to */public/*.
" this is known to have caused problems with spec/vcr_cassettes/ directory:
" when refreshing it in nerdtree directory kept on collapsing all the time
set wildignore+=*.log,public/*,spec/vcr_cassettes/*,tmp/*,deps/*,node_modules/*

"-------------------------------------------------------------------------------
" diff
"-------------------------------------------------------------------------------

" show filler lines, use vertical split by default
set diffopt=filler,vertical

"-------------------------------------------------------------------------------
" editing
"-------------------------------------------------------------------------------

set colorcolumn=81
"execute 'set colorcolumn=' . join(range(81,250), ',')
set cpoptions+=$

"-------------------------------------------------------------------------------
" folding
" http://vimcasts.org/episodes/how-to-fold/
"-------------------------------------------------------------------------------

set foldenable
set foldlevelstart=10
" syntax folding might not work in some cases (e.g. in spec files)
set foldmethod=indent
set foldnestmax=10

"-------------------------------------------------------------------------------
" formatting
"-------------------------------------------------------------------------------

" NOTE: these options might be overriden by filetype plugins!
"
" t - auto-wrap text using textwidth
" c - auto-wrap comments using textwidth
" r - insert comment leader after <CR> in insert mode
" o - insert comment leader after 'o' or 'O' in normal mode
" q - allow formatting of comments with 'gq'
" l - don't break long lines in insert mode
" j - remove comment leader when joining lines
set formatoptions-=tc
set formatoptions+=rj

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

"-------------------------------------------------------------------------------
" visualize invisible characters
"-------------------------------------------------------------------------------

set list
set listchars=nbsp:·,tab:▸·,trail:·,precedes:«,extends:»

"=============================================================================="
"                                                                              "
" look and feel                                                                "
"                                                                              "
"=============================================================================="

" disable beeping
set vb t_vb=

"-------------------------------------------------------------------------------
" colors
"-------------------------------------------------------------------------------

set transparency=0

"let g:solarized_bold = 1
"let g:solarized_contrast = 'normal'
"let g:solarized_diffmode = 'normal'
"let g:solarized_italic = 0
"let g:solarized_underline = 1

"colorscheme PaperColor
"colorscheme github
"colorscheme hybrid-light
"colorscheme ir_black_morr
"colorscheme ir_black_tap
"colorscheme scheakur
"colorscheme sebocean_dasha
"colorscheme solarized
"colorscheme summerfruit_tap
"colorscheme vylight

if hostname() == 'MacBook-Pro-Personal.local'
  set background=light
  colorscheme PaperColor
else
  set background=light
  colorscheme PaperColor
endif

"-------------------------------------------------------------------------------
" font
"-------------------------------------------------------------------------------

"set linespace=6
"set guifont=Andale\ Mono:h14
"set linespace=2
"set guifont=Andale\ Mono\ MT\ Std:h14
"set linespace=4
"set guifont=Cousine:h14
"set linespace=7
"set guifont=Droid\ Sans\ Mono\ for\ Powerline:h14
"set linespace=5
"set guifont=Fira\ Mono:h14
"set linespace=0
"set guifont=Inconsolata-dz\ For\ Powerline:h14
"set linespace=0
"set guifont=Inconsolata\ LGC\ For\ Powerline:h14
"set linespace=2
"set guifont=Input\ Mono\ Narrow:h13
"set linespace=6
"set guifont=MonacoB\ for\ Powerline:h13

if hostname() == 'MacBook-Pro-Personal.local'
  set linespace=5
  set guifont=Andale\ Mono\ MT\ Std:h14
else
  set linespace=5
  set guifont=Andale\ Mono\ MT\ Std:h14
endif

"-------------------------------------------------------------------------------
" gui options
"-------------------------------------------------------------------------------

" NOTE: flags 'rL' must be added in exactly this order (IDK why)!
"
" c - use console dialogs instead of gui ones
" r - right scrollbar is always present
" L - left scrollbar is present when there's a vertically split window
set guioptions+=c
set guioptions-=rL

" disable cursor blinking
set guicursor+=a:blinkon0

"-------------------------------------------------------------------------------
" indicators
"-------------------------------------------------------------------------------

" highlight current line (might slow down navigation)
" this is the main cause of sluggish scrolling!
" set after colorscheme - or else it can be redefined inside it
set nocursorline
" hide mode indicator
set noshowmode
" show number of lines or characters selected in bottom right corner
set showcmd
" always show tabbar
set showtabline=2

" show line numbering
if hostname() == 'MacBook-Pro-Personal.local'
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
set shell=/bin/zsh
" http://stackoverflow.com/a/4642855/3632318
" (see last comment - using interactive shell can create problems)
"set shellcmdflag=-ic

" maximize on startup
" NOTE: setting columns to 999 maximizes the 1st column when using vimdiff!
"set lines=999
"set columns=999

"===============================================================================
"                                                                              =
" abbreviations                                                                =
"                                                                              =
"===============================================================================

iabbrev ii i18n_i
iabbrev lasnt last
iabbrev teh the
iabbrev tt i18n_t

"===============================================================================
"                                                                              =
" autocommands                                                                 =
"                                                                              =
" exclamation mark means to remove all autocommands associated with event,     =
" pattern and command - the point here is that by default autocommands are     =
" are accumulated every time .vimrc is sourced.                                =
"                                                                              =
"===============================================================================

" https://github.com/vim-airline/vim-airline/issues/539
"
" refresh airline 2 times after sourcing vimrc:
" to redraw statusline itself (1st call) and tabline (2nd call).
" don't forget to refresh airline after sourcing vimrc manually
augroup vimrc
  autocmd!
  autocmd BufWritePost $MYVIMRC source $MYVIMRC | AirlineRefresh | AirlineRefresh
augroup END

let s:prevtabnr = tabpagenr()
let s:prevtabcount = tabpagenr('$')
augroup tabs
  autocmd!
  autocmd TabEnter * call s:GoToPrevTab()
augroup END

augroup filetypes
  autocmd!
  autocmd BufRead,BufNewFile *.arb setlocal filetype=ruby
  autocmd BufRead,BufNewFile *.jb setlocal filetype=ruby
  " using rspec filetype for specs doesn't change highlighting
  " at all but prevents rubocop checker from running on them
  "autocmd BufRead,BufNewFile *_spec.rb set filetype=rspec
augroup END

" used to disable <CR> in quickfix window - now it's
" not necessary because QFEnter mapping already does it
"augroup quickfix
"  autocmd!
"  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
"augroup END

" I don't remember why I added this in the first place -
" probably airline didn't work correctly in diff mode.
" however now everything seems to be okay -> comment it out
"augroup diffmode
"  autocmd!
"  autocmd FilterWritePre * call SetDiffMode()
"augroup END

augroup backup
  autocmd!
  autocmd BufWritePre * call SetBackupDir()
augroup END

"===============================================================================
"                                                                              =
" mappings                                                                     =
"                                                                              =
" http://vim.wikia.com/wiki/Mapping_keys_in_Vim_-_Tutorial_(Part_1)            =
"                                                                              =
" NOTE: don't use noremap for plugin mappings                                  =
"                                                                              =
"===============================================================================

" Leader: global and plugin mappings
" LocalLeader: mappings local to current buffer (<buffer> mappings)

let mapleader = ','
let maplocalleader = '\'

" it must allow to use option (alt) in most mappings
" but it breaks mappings for moving tabs (:tabmove)
"set macmeta

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
" edit without leaving normal mode
"-------------------------------------------------------------------------------

" delete trailing whitespaces
nnoremap <silent> <Leader>dt :%s/\s\+$//<CR>:w<CR>:nohlsearch<CR>
" insert newline after current line
nmap <silent> <CR> o<Esc>
" insert newline before current line
nmap <silent> <S-CR> O<Esc>
" insert space
nmap <silent> <Space> i<Space><Esc>l

"-------------------------------------------------------------------------------
" fold
"-------------------------------------------------------------------------------

nnoremap <Backspace> za

"-------------------------------------------------------------------------------
" open popular files
"-------------------------------------------------------------------------------

nnoremap <Leader>ov :tabnew<CR>:edit $MYVIMRC<CR>
nnoremap <Leader>oz :tabnew<CR>:edit ~/.zshenv<CR>

"-------------------------------------------------------------------------------
" fullscreen
"-------------------------------------------------------------------------------

" doesn't work this way - change toggle fullscreen mode key in
" System Preferences -> Keyboard -> Shortcuts -> App Shortcuts
"macmenu Window.Toggle\ Full\ Screen\ Mode key=<nop>

"-------------------------------------------------------------------------------
" navigation
"-------------------------------------------------------------------------------

" move vertically by visual line - not real one
" (comment out so far - I don't use line wrapping)
"nnoremap j gj
"nnoremap k gk

nnoremap <C-j> 10j
nnoremap <C-k> 10k

" g_ (unlike $) doesn't select newline character in visual mode
nnoremap H ^
nnoremap L g_

" switch to last active tab
let g:lasttabnr = 1
nnoremap <silent> <C-Tab> :exec 'tabnext ' . g:lasttabnr<CR>
autocmd! TabLeave * let g:lasttabnr = tabpagenr()

"------- buffer ----------------------------------------------------------------

" same as <C-6>
nnoremap <silent> <C-s> :b#<CR>

"nnoremap <silent> <C-p> :bprevious<CR>
"nnoremap <silent> <C-n> :bnext<CR>

"------- mark history ----------------------------------------------------------

" this mapping must come before remapping <Tab>
nnoremap <C-g> <C-i>

"------- tab -------------------------------------------------------------------

" same as using gT and gt
nnoremap <C-h> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>

nmap <silent> ˙ :tabmove -1<CR>
nmap <silent> ¬ :tabmove +1<CR>

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
"
" :help user-commands
" call function: command! MyFunction call MyFunction()
"-------------------------------------------------------------------------------

command! EncodeInWindows1251 :edit ++encoding=cp1251<CR>
command! EncodeInUTF8 :edit ++encoding=utf-8<CR>

"-------------------------------------------------------------------------------
" save
"-------------------------------------------------------------------------------

nnoremap <silent> <Tab> :w<CR>

"-------------------------------------------------------------------------------
" search
"-------------------------------------------------------------------------------

" turn off highlighting and clear messages
" <C-c> in normal mode aborts any pending command
nnoremap <silent> <C-c> <C-c>:nohlsearch<Bar>:echo<CR>
nnoremap <silent> <C-Backspace> :nohlsearch<Bar>:echo<CR>

"-------------------------------------------------------------------------------
" sourcing configuration files
"-------------------------------------------------------------------------------

" see comments above about AirlineRefresh
nnoremap <Leader>.v :source $MYVIMRC<CR>:AirlineRefresh<CR>

"-------------------------------------------------------------------------------
" suppress unwanted keys (set to noop)
"-------------------------------------------------------------------------------

nnoremap Q <nop>
nnoremap K <nop>

"-------------------------------------------------------------------------------
" yanking
"-------------------------------------------------------------------------------

nnoremap Y y$

"-------------------------------------------------------------------------------
" useful maps
"-------------------------------------------------------------------------------

" TODO: function to toggle focusing in rspec

" http://superuser.com/a/382582
" http://vim.wikia.com/wiki/Selecting_your_pasted_text
"
" highlight last inserted or pasted text (works till save):
" the last character of previously inserted text is the one right
" after inserted text -> this command selects one character more
" than actually inserted (though it's all okay for pasted text)
nnoremap gp `[v`]

"===============================================================================
" insert mode                                                                  =
"===============================================================================

inoremap <C-Backspace> <Esc>
" map to Esc for vertical editing to work
inoremap <C-c> <Esc>

"-------------------------------------------------------------------------------
" editing
"-------------------------------------------------------------------------------

inoremap <C-d> <Delete>

"-------------------------------------------------------------------------------
" generation of complementary characters
"-------------------------------------------------------------------------------

inoremap {{ {<Space><Space>}<Esc>hi
inoremap }} {}<Esc>i
"inoremap )) ()<Esc>i
"inoremap >> \|><Space>
"inoremap ]] []<Esc>i

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

vnoremap <C-Backspace> <Esc>

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
" indenting
" http://stackoverflow.com/a/444461
" http://stackoverflow.com/a/1413854
"
" alternatively use:
" . - repeat indenting
" u - undo indenting
" gv - restore last visual block
"-------------------------------------------------------------------------------

"vnoremap < <gv
"vnoremap > >gv

"-------------------------------------------------------------------------------
" searching
"-------------------------------------------------------------------------------

vnoremap * y/<C-r>"<CR>

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
"
" I use my fork: https://github.com/tap349/ag.vim
" (removed all default keybindings and use QFEnter keybindings instead)
"-------------------------------------------------------------------------------

" ignore the same directories as in wildignore:
" ~/.agignore ignore file is used by default or else
" it can be specified with `--path-to-ignore` ag option
let g:ag_prg = 'ag --vimgrep --literal'
let g:ag_working_path_mode = 'r'

" don't jump to first found file
map <Leader>/ :Ag!<Space>

"-------------------------------------------------------------------------------
" CamelCaseMotion
"-------------------------------------------------------------------------------

map <silent> w <Plug>CamelCaseMotion_w
map <silent> e <Plug>CamelCaseMotion_e
map <silent> b <Plug>CamelCaseMotion_b

"-------------------------------------------------------------------------------
" command-t
"-------------------------------------------------------------------------------

"let g:CommandTCancelMap = '<C-c>'
"let g:CommandTMatchWindowAtTop = 0
"let g:CommandTMatchWindowReverse = 0
"let g:CommandTMaxHeight = 17
"let g:CommandTMaxFiles = 25000
"let g:CommandTWildIgnore = &wildignore . 'public/**,tmp/**,**/*.log'

"nmap <F1> :CommandT<CR>
"imap <F1> <Esc>:CommandT<CR>
"nmap <S-F1> :CommandTBuffer<CR>
"nmap <Leader><F1>r :CommandTFlush<CR>:CommandT<CR>

"" http://vimdoc.sourceforge.net/htmldoc/usr_40.html
"command! -nargs=+ GotoOrOpenTab call GotoOrOpenTab('<args>')
"let g:CommandTAcceptSelectionTabCommand = 'GotoOrOpenTab'

"-------------------------------------------------------------------------------
" ctrlp.vim
"-------------------------------------------------------------------------------

" instant update causes cursor to appear and flicker
" at the end of last line in match window
let g:ctrlp_lazy_update = 5
let g:ctrlp_map = '<Leader>s'
let g:ctrlp_match_window = 'order:ttb,max:15'
let g:ctrlp_mruf_relative = 1
let g:ctrlp_root_markers = ['mix.exs']
let g:ctrlp_switch_buffer = 'et'
" you might consider turning off caching at all when using ag
let g:ctrlp_use_caching = 1
" it's not possible to use g:ag_prg variable here - options differ.
" add `-g ""` to print filenames (otherwise nothing is found)
let g:ctrlp_user_command = 'ag %s --files-with-matches -g ""'

let g:ctrlp_prompt_mappings = {
  \ 'PrtDeleteWord()':    ['<C-w>'],
  \ 'PrtClear()':         ['<C-u>'],
  \ 'PrtSelectMove("j")': ['<C-n>', 'Down'],
  \ 'PrtSelectMove("k")': ['<C-p>', 'Up'],
  \ 'PrtHistory(-1)':     ['<C-j>'],
  \ 'PrtHistory(1)':      ['<C-k>'],
  \ 'ToggleType(1)':      ['<C-l>'],
  \ 'ToggleType(-1)':     ['<C-h>'],
  \ 'PrtExpandDir()':     ['<Tab>'],
  \ 'PrtInsert()':        ['<C-\>'],
  \ 'PrtCurStart()':      ['<C-a>'],
  \ 'PrtCurEnd()':        ['<C-e>'],
  \ 'PrtCurLeft()':       ['<C-b>', '<Left>'],
  \ 'PrtCurRight()':      ['<C-f>', '<Right>'],
  \ 'PrtClearCache()':    ['<C-r>'],
  \ 'CreateNewFile()':    ['<C-y>'],
  \ 'OpenMulti()':        ['<C-o>'],
  \ 'PrtExit()':          ['<Esc>', '<C-c>', '<C-g>']
  \ }

let g:ctrlp_buffer_func = {
  \ 'enter': 'BrightHighlightOn',
  \ 'exit':  'BrightHighlightOff'
  \ }

function! BrightHighlightOn()
  hi CursorLine guibg=#d7e2ea
endfunction

function! BrightHighlightOff()
  hi CursorLine guibg=#e4e4e4
endfunction

"-------------------------------------------------------------------------------
" ctrlp-modified.vim
"-------------------------------------------------------------------------------

" do not use so far - see issue in 2do
"map <Leader>m :CtrlPModified<CR>
"map <Leader>M :CtrlPBranch<CR>

"-------------------------------------------------------------------------------
" gitv
"-------------------------------------------------------------------------------

"" disable mappings with control key - otherwise I can't use <C-l>
"let g:Gitv_DoNotMapCtrlKey = 1
"" don't set to 0 - opening preview window for commit results in
"" error unless preview window has been opened from the very beginning
"let g:Gitv_OpenPreviewOnLaunch = 1
"let g:Gitv_WipeAllOnClose = 1

"-------------------------------------------------------------------------------
" lightline.vim
"-------------------------------------------------------------------------------

"set laststatus=2

"let g:lightline = {
"  \ 'colorscheme': 'solarized',
"  \ 'active': {
"  \   'left': [['mode'], ['fugitive', 'filename']]
"  \ },
"  \ 'component_function': {
"  \   'fugitive': 'LightLineFugitive',
"  \   'filename': 'LightLineFilename',
"  \   'ctrlpmark': 'CtrlPMark',
"  \   'readonly': 'LightLineReadonly',
"  \   'modified': 'LightLineModified'
"  \ },
"  \ 'separator': { 'left': '⮀', 'right': '⮂' },
"  \ 'subseparator': { 'left': '⮁', 'right': '⮃' }
"  \ }

"function! LightLineFugitive()
"  return exists('*fugitive#head') ? fugitive#head() : ''
"endfunction

"function! LightLineFilename()
"  return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
"       \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
"       \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
"endfunction

"function! LightLineReadonly()
"  if &filetype == 'help'
"    return ''
"  elseif &readonly
"    return '⭤'
"  else
"    return ''
"  endif
"endfunction

"function! LightLineModified()
"  if &filetype == 'help'
"    return ''
"  elseif &modified
"    return '+'
"  elseif &modifiable
"    return ''
"  else
"    return ''
"  endif
"endfunction

"-------------------------------------------------------------------------------
" nerdcommenter
"
" <C-1>,<C-2>,etc. are not allowed as well as <C-/>. see for details:
" http://vim.1045645.n5.nabble.com/mapping-control-0-1-or-backtick-td1189910.html
"-------------------------------------------------------------------------------

let g:NERDSpaceDelims = 0
let g:NERDDefaultAlign = 'left'

map <Leader><Space> <Plug>NERDCommenterToggle

"-------------------------------------------------------------------------------
" nerdtree
"-------------------------------------------------------------------------------

" fix ugly arrows from Andale Mono MT
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

" don't collaps dirs that have only one child
let NERDTreeCascadeSingleChildDir = 0

nmap <F1> :NERDTreeFind<CR>
nmap <F2> :NERDTreeToggle<CR>

"-------------------------------------------------------------------------------
" QFEnter
"
" NOTE: QFEnter respects `switchbuf` option! if selected file is opened
"       in another tab all mappings below just switch to that tab
"-------------------------------------------------------------------------------

let g:qfenter_open_map = ['<CR>']
let g:qfenter_topen_map = ['<C-t>']
let g:qfenter_hopen_map = ['<C-s>']
let g:qfenter_vopen_map = ['<C-v>']

"-------------------------------------------------------------------------------
" supertab
"-------------------------------------------------------------------------------

let g:SuperTabCrMapping = 1
let g:SuperTabDefaultCompletionType = '<C-n>'

" used only when completeopt has 'longest' option
let g:SuperTabLongestEnhanced = 1
let g:SuperTabLongestHighlight = 1

"-------------------------------------------------------------------------------
" syntastic
" https://github.com/scrooloose/syntastic/blob/master/doc/syntastic.txt
"-------------------------------------------------------------------------------

" show syntastic errors in separate window
nmap <silent> <Leader>e :Errors<CR>

let g:syntastic_ruby_mri_exec = '~/.rbenv/shims/ruby'
let g:syntastic_ruby_rubocop_exec = '~/.rbenv/shims/rubocop'

" NOTE: using rubocop checker on every buffer save might be very slow:
"let g:syntastic_ruby_checkers = ['mri', 'rubocop']

let g:syntastic_coffee_checkers = []
let g:syntastic_ruby_checkers = []
let g:syntastic_sass_checkers = []
let g:syntastic_slim_checkers = []

" ruby rubocop checker (gem install rubocop)
nmap <silent> <Leader>r :SyntasticCheck ruby rubocop<CR>

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
" https://github.com/vim-airline/vim-airline/blob/master/doc/airline.txt
"-------------------------------------------------------------------------------

" always show airline
set laststatus=2

" NOTE: clicking on left_sep selects a tab to the left
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_tabs = 1
let g:airline#extensions#tabline#show_tab_nr = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ' '
"let g:airline#extensions#tabline#left_sep = '⮀'
"let g:airline#extensions#tabline#left_alt_sep = '⮁'
"let g:airline#extensions#tabline#right_sep = '⮂'
"let g:airline#extensions#tabline#right_alt_sep = '⮃'

let g:airline_powerline_fonts = 0

"let g:airline_theme = 'cool'
"let g:airline_theme = 'sol'
"let g:airline_theme = 'silver'
let g:airline_theme = 'lucius'

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

let g:airline_left_sep = '⮀'
let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
let g:airline_right_alt_sep = '⮃'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.crypt = '🔒'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.maxlinenr = '☰'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.readonly = '⭤'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.whitespace = 'Ξ'

"-------------------------------------------------------------------------------
" vim-buffergator
"-------------------------------------------------------------------------------

"let g:buffergator_sort_regime = 'basename'
"let g:buffergator_split_size = 20
let g:buffergator_suppress_keymaps = 1
"let g:buffergator_viewport_split_policy = 'R'
"let g:buffergator_vsplit_size = 60

nmap <F3> :BuffergatorToggle<CR>
nmap <silent> <C-p> :BuffergatorMruCyclePrev<CR>
nmap <silent> <C-n> :BuffergatorMruCycleNext<CR>

"-------------------------------------------------------------------------------
" vim-easymotion
"-------------------------------------------------------------------------------

let g:EasyMotion_enter_jump_first = 1

" https://github.com/easymotion/vim-easymotion#default-bindings
map <Leader> <Plug>(easymotion-prefix)

"nmap <Leader>s <Plug>(easymotion-s2)
nmap <Leader>f <Plug>(easymotion-f)
nmap <Leader>F <Plug>(easymotion-F)
nmap <Leader>w <Plug>(easymotion-bd-w)

"-------------------------------------------------------------------------------
" vim-extradite
"-------------------------------------------------------------------------------

" disable resizing windows
" (for some reason maximizes width of nerdtree window when it's open)
let g:extradite_resize = 0
let g:extradite_showhash = 1

map <silent> <Leader>g :Extradite<CR>

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
" vim-gutentags
" https://andrew.stwrt.ca/posts/vim-ctags/
" https://github.com/mmorearty/elixir-ctags
"
" by default `ctags` command generates _tags_ file in project root
" (gutentags does the same since it's using `ctags`).
" when gutentags_cache_dir is set it will be used to store tagfiles
" for all projects like this: _Users-tap-edev-rumbl-tags_
"
" <C-]> - jump to tag
" <C-t> - go back in tag stack (previous tag or original place)
"
" don't use comments in ~/.ctags configuration file -
" or else configuration from this file won't be used by ctags
"
" sometimes tags are not found unexpectedly (even though they used
" to be found before) - closing and opening vim helps in this case
"-------------------------------------------------------------------------------

" show TAGS when indexing is in progress
"set statusline+=%{gutentags#statusline()}

let g:gutentags_cache_dir = '~/.vim/tags'
let g:gutentags_enabled = 1
" default project root markers are appended to this list
" (probably gutentags can also use g:ctrlp_root_markers).
" don't use .gitignore as project root marker because of ~/.gitignore
" (tags for all files within home directory will be created)
let g:gutentags_project_root = ['mix.exs']

"-------------------------------------------------------------------------------
" vim-hugefile
"-------------------------------------------------------------------------------

let g:hugefile_trigger_size = 1

"-------------------------------------------------------------------------------
" vim-markdown
"-------------------------------------------------------------------------------

"let g:vim_markdown_folding_disabled = 0
"let g:vim_markdown_folding_level = 1
let g:vim_markdown_folding_style_pythonic = 1
let g:vim_markdown_frontmatter = 1

"-------------------------------------------------------------------------------
" vim-polyglot
"-------------------------------------------------------------------------------

" using this plugin causes noticeable delay
" when opening new buffer with coffee file -
" highlighting still works without it
"let g:polyglot_disabled = ['coffee']

"-------------------------------------------------------------------------------
" vim-rails
"
" example projections: https://gist.github.com/henrik/5676109
"-------------------------------------------------------------------------------

" https://github.com/tpope/vim-rails/issues/443
set confirm

nmap <Leader>, :A<CR>
nmap <Leader>v :AV<CR>

" the first projection is for `set confirm` to work in app/ directory
let g:rails_projections = {
  \   'app/*.rb': {
  \     'alternate': 'spec/{}_spec.rb'
  \   },
  \   'app/admin/*.rb': {
  \     'alternate': 'spec/controllers/admin/{}_controller_spec.rb'
  \   },
  \   'spec/controllers/admin/*_controller_spec.rb': {
  \     'alternate': 'app/admin/{}.rb'
  \   },
  \   'lib/*.rb': {
  \     'alternate': 'spec/{}_spec.rb'
  \   },
  \   'spec/*_spec.rb': {
  \     'alternate': 'lib/{}.rb'
  \   },
  \   'config/locales/*ru.yml': {
  \     'alternate': 'config/locales/{}en.yml'
  \   },
  \   'config/locales/*en.yml': {
  \     'alternate': 'config/locales/{}ru.yml'
  \   }
  \ }

"-------------------------------------------------------------------------------
" vim-session
"-------------------------------------------------------------------------------

"let g:session_autoload = 'no'
"let g:session_autosave = 'no'

"nmap <F7>d :DeleteSession<Space>
"nmap <F7>o :OpenSession<Space>
"nmap <F7>s :SaveSession<Space>

"===============================================================================
"                                                                              =
" common functions                                                             =
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
"function! BufHidden(buf)
"  let active_buffers = []
"  let tabs = range(1, tabpagenr('$'))

"  call map(tabs, 'extend(active_buffers, tabpagebuflist(v:val))')
"  return (bufexists(a:buf) && index(active_buffers, a:buf) == -1)
"endfunction

" a:000: http://learnvimscriptthehardway.stevelosh.com/chapters/24.html
" sbuffer: `buffer` function doesn't respect `switchbuf` option
"function! GotoOrOpenTab(...)
"  for file in a:000
"    " if buffer exists and not hidden
"    if bufexists(file) && !BufHidden(bufnr(file))
"      exec 'sbuffer ' . file
"    else
"      exec 'tabedit ' . file
"    endif
"  endfor
"endfunction

" settings for difftool and mergetool
"function! SetDiffMode()
"  if &diff
"    set background=light
"    colorscheme summerfruit_tap

"    " don't show airline (it's not properly initialized at this moment)
"    set laststatus=0
"  endif
"endfunction

function! SetBackupDir()
  let l:backupdir = $HOME . '/.vim/backup' . expand('%:p:h')
  if !isdirectory(l:backupdir)
    call mkdir(l:backupdir, 'p', 0700)
  endif

  let &backupdir = l:backupdir
  let &backupext = strftime('~(%Y-%m-%d %H:%M:%S)')
endfunction
