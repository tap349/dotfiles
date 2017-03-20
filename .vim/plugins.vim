"===============================================================================
"                                                                              =
" plugins                                                                      =
"                                                                              =
" NOTE: don't use noremap for plugin mappings                                  =
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

Plug 'tap349/vim-airline'
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
" always run from current working directory (default)
"let g:ag_working_path_mode = 'r'

" - (L) use location list instead of quickfix window
"   (allows to have different searches in different tabs)
" - (!) don't jump to first found file
"
" QFEnter works with both quickfix windows and location lists
map <Leader>/ :LAg!<Space>

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
" https://github.com/vim-airline/vim-airline/issues/1448
"
" setting 'bottom,order:ttb' prevents from showing vim-airline statusline right
" after closing ctrlp window (all other combinations of position and order work)
"
" set min and max to the same value to prevent jumping
let g:ctrlp_match_window = 'bottom,order:ttb,min:14,max:14'
let g:ctrlp_mruf_relative = 1
let g:ctrlp_root_markers = ['mix.exs']
let g:ctrlp_switch_buffer = 'et'
" you might consider turning off caching at all when using ag
let g:ctrlp_use_caching = 1
" it's not possible to use g:ag_prg variable here - options differ.
" add `-g ""` to print filenames (otherwise nothing is found)
let g:ctrlp_user_command = 'ag %s --files-with-matches -g ""'
" don't set working directory with ctrlp
let g:ctrlp_working_path_mode = 0

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
" QFEnter respects `switchbuf` option! if selected file is opened
" in another tab all mappings below just switch to that tab
"-------------------------------------------------------------------------------

" disable automatic opening of quickfix window (or location list)
" when opening file from current quickfix window in a new tab
let g:qfenter_enable_autoquickfix = 0

let g:qfenter_keymap = {}
let g:qfenter_keymap.open = ['<CR>']
let g:qfenter_keymap.open_keep = ['<S-CR>']
let g:qfenter_keymap.hopen = ['<C-s>']
let g:qfenter_keymap.vopen = ['<C-v>']
let g:qfenter_keymap.topen = ['<C-t>']

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
"-------------------------------------------------------------------------------

" show syntastic errors in separate window
nmap <silent> <Leader>e :Errors<CR>

let g:syntastic_ruby_mri_exec = '~/.rbenv/shims/ruby'
let g:syntastic_ruby_rubocop_exec = '~/.rbenv/shims/rubocop'

" using rubocop checker on every buffer save might be very slow
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
