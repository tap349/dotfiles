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
Plug 'itchyny/lightline.vim' | Plug 'tpope/vim-fugitive'
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
"Plug 'jamessan/vim-gnupg'
"Plug 'scheakur/vim-scheakur'
"Plug 'xolox/vim-session'

" plugins to try in the future

"Plug 'Shougo/vimfiler.vim'
"Plug 'Shougo/vimshell.vim'

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
let g:ctrlp_match_window = 'bottom,order:ttb,max:15'
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
      \   'PrtDeleteWord()':    ['<C-w>'],
      \   'PrtClear()':         ['<C-u>'],
      \   'PrtSelectMove("j")': ['<C-n>', 'Down'],
      \   'PrtSelectMove("k")': ['<C-p>', 'Up'],
      \   'PrtHistory(-1)':     ['<C-j>'],
      \   'PrtHistory(1)':      ['<C-k>'],
      \   'ToggleType(1)':      ['<C-l>'],
      \   'ToggleType(-1)':     ['<C-h>'],
      \   'PrtExpandDir()':     ['<Tab>'],
      \   'PrtInsert()':        ['<C-\>'],
      \   'PrtCurStart()':      ['<C-a>'],
      \   'PrtCurEnd()':        ['<C-e>'],
      \   'PrtCurLeft()':       ['<C-b>', '<Left>'],
      \   'PrtCurRight()':      ['<C-f>', '<Right>'],
      \   'PrtClearCache()':    ['<C-r>'],
      \   'CreateNewFile()':    ['<C-y>'],
      \   'OpenMulti()':        ['<C-o>'],
      \   'PrtExit()':          ['<Esc>', '<C-c>', '<C-g>']
      \ }

let g:ctrlp_buffer_func = {
      \   'enter': 'CtrlPBufferFunc_1',
      \   'exit':  'CtrlPBufferFunc_2'
      \ }

function! CtrlPBufferFunc_1()
  hi CursorLine guibg=#d7e2ea
  call lightline#update_once()
endfunction

function! CtrlPBufferFunc_2()
  hi CursorLine guibg=#e4e4e4
endfunction

let g:ctrlp_status_func = {
      \ 'main': 'CtrlPStatusFunc_1',
      \ 'prog': 'CtrlPStatusFunc_2'
      \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
  let g:lightline.ctrlp_regex = a:regex
  let g:lightline.ctrlp_prev = a:prev
  let g:lightline.ctrlp_item = a:item
  let g:lightline.ctrlp_next = a:next
  return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
  return lightline#statusline(0)
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

set laststatus=2
" disable GUI tab pages line
set guioptions-=e

let g:lightline = {}
let g:lightline.enable = { 'statusline': 1, 'tabline': 1 }
let g:lightline.colorscheme = 'lucius'
" https://github.com/itchyny/lightline.vim/issues/220
let g:lightline.winwidth = 200

" statusline

"let g:lightline.separator = { 'left': '⮀', 'right': '⮂' }
"let g:lightline.subseparator = { 'left': '⮁', 'right': '⮃' }
let g:lightline.separator = { 'left': '', 'right': '' }
let g:lightline.subseparator = { 'left': '', 'right': '' }

let g:lightline.mode_map = {
      \ 'n' : 'N',
      \ 'i' : 'I',
      \ 'R' : 'R',
      \ 'v' : 'V',
      \ 'V' : 'VL',
      \ "\<C-v>": 'VB'
      \ }

let g:lightline.active = {
\   'left': [['mode'], ['fugitive', 'ctrlpitem'], ['filename']],
\   'right': [['syntastic', 'lineinfo'], ['filetype']]
\ }
let g:lightline.inactive = {
\   'left': [['filename']],
\   'right': [['syntastic', 'lineinfo'], ['filetype']]
\ }

" tabline

let g:lightline.tabline = { 'left': [['tabs']], 'right': [['close']] }
let g:lightline.tabline_separator = { 'left': '', 'right': '' }
let g:lightline.tabline_subseparator = { 'left': '', 'right': '' }
let g:lightline.tab = {
\   'active': ['filename', 'modified'],
\   'inactive': ['filename', 'modified']
\ }

" components

let g:lightline.component = {
      \   'fileencodingformat': '%{&fenc !=# "" ? &fenc : &enc}[%{&ff}]'
      \ }
let g:lightline.component_function = {
      \   'mode': 'LightlineMode',
      \   'fugitive': 'LightlineFugitive',
      \   'ctrlpitem': 'LightlineCtrlPItem',
      \   'filename': 'LightlineFilename',
      \   'ctrlpmark': 'LightlineCtrlPMark',
      \   'filetype': 'LightlineFiletype',
      \   'lineinfo': 'LightlineLineinfo'
      \ }
let g:lightline.component_expand = {
      \   'syntastic': 'SyntasticStatuslineFlag'
      \ }
let g:lightline.component_type = {
      \   'syntastic': 'warning'
      \ }

" functions for components
"
" &enc = &encoding
" &fenc = &fileencoding
" &ft = &filetype
" &ma = &modifiable
" &mod = &modified
" &ro = &readonly

function! LightlineMode()
  return s:IsNerdTree() ? 'NERD' :
        \ s:IsCtrlP() ? 'CtrlP' :
        \ s:IsNarrowWindow() ? '' : lightline#mode()
endfunction

function! LightlineFugitive()
  if s:IsNotebookWindow() | return '' | end
  if s:IsNarrowWindow() | return '' | end
  if !exists('*fugitive#head') | return '' | end

  let branch = fugitive#head()
  return branch !=# '' ? '⎇ ' . branch : ''
endfunction

function! LightlineCtrlPItem()
  if !s:IsCtrlP() | return '' | end

  " :help g:ctrlp_status_func
  " g:lightline.ctrlp_regex: 0 - not regex mode, 1 - regex mode
  call lightline#link('nR'[g:lightline.ctrlp_regex])
  return g:lightline.ctrlp_item
endfunction

function! LightlineFilename()
  if s:IsNerdTree() | return '' | end
  if s:IsCtrlP() | return '' | end

  let fname = expand('%:t')
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ ('' != fname ? fname : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineReadonly()
  return &ft !~? 'help' && &ro ? '⭤' : ''
endfunction

function! LightlineModified()
  return &ma && &mod ? '+' : ''
endfunction

function! LightlineCtrlPMark()
  if !s:IsCtrlP() | return '' | end

  let search_modes = [
        \   g:lightline.ctrlp_prev,
        \   g:lightline.ctrlp_item,
        \   g:lightline.ctrlp_next
        \ ]
  return lightline#concatenate(search_modes, 0)
endfunction

function! LightlineFiletype()
  if s:IsNotebookWindow() | return '' | end
  if s:IsCtrlP() | return '' | end

  return &ft != '' ? &ft : 'no ft'
endfunction

function! LightlineLineinfo()
  if s:IsNarrowWindow() | return '' | end
  if s:IsCtrlP() | return '' | end

  return printf('%3d/%d☰ : %-2d', line('.'), line('$'), col('.'))
endfunction

function! s:IsNerdTree()
  " g:lightline.ctrlp_item must be set in ctrlp configuration:
  " has_key(g:lightline, 'ctrlp_item') must return 1
  return expand('%:t') =~ 'NERD_tree'
endfunction

function! s:IsCtrlP()
  " g:lightline.ctrlp_item must be set in ctrlp configuration:
  " has_key(g:lightline, 'ctrlp_item') must return 1
  return expand('%:t') == 'ControlP'
endfunction

function! s:IsNarrowWindow()
  return winwidth(0) <= 60
endfunction

function! s:IsNotebookWindow()
  return winwidth(0) <= 90
endfunction

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

nmap <silent> <F1> :NERDTreeFind<CR>
nmap <silent> <F2> :NERDTreeToggle<CR>

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
"nmap <silent> <Leader>e :Errors<CR>

let g:syntastic_ruby_mri_exec = '~/.rbenv/shims/ruby'
let g:syntastic_ruby_rubocop_exec = '~/.rbenv/shims/rubocop'

" automatically run checkers for different filetypes
" (they are run on every buffer save which might be very slow)
let g:syntastic_coffee_checkers = []
"let g:syntastic_ruby_checkers = ['mri', 'rubocop']
let g:syntastic_ruby_checkers = []
let g:syntastic_sass_checkers = []
let g:syntastic_slim_checkers = []

nmap <silent> <Leader>r :call <SID>MySyntasticCheck()<CR>

function! s:MySyntasticCheck()
  SyntasticCheck mri rubocop
  call lightline#update()
endfunction

" http://vim.wikia.com/wiki/Simplifying_regular_expressions_using_magic_and_no-magic
" '\m^shadowing outer local variable'
let g:syntastic_ruby_mri_quiet_messages = {
\   'regex': [
\     '\m`&'' interpreted as argument prefix',
\     '\m`*'' interpreted as argument prefix',
\     '\mambiguous first argument; put parentheses or a space even after `/'' operator'
\   ]
\ }

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