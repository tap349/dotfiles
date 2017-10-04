"*******************************************************************************
"
" plugins
"
" NOTE: don't use noremap for plugin mappings
"
"*******************************************************************************

"===============================================================================
"
" installation
"
"===============================================================================

call plug#begin('~/.vim/plugged')

"-------------------------------------------------------------------------------
" ctrlp
"-------------------------------------------------------------------------------

Plug 'ctrlpvim/ctrlp.vim'
Plug 'jasoncodes/ctrlp-modified.vim'
" https://www.reddit.com/r/vim/comments/38m0g7/cpsm_a_ctrlp_matcher/
Plug 'nixprime/cpsm'

"-------------------------------------------------------------------------------
" elixir / phoenix
"-------------------------------------------------------------------------------

Plug 'elixir-lang/vim-elixir'
Plug 'andyl/vim-projectionist-elixir' | Plug 'tpope/vim-projectionist'
" https://github.com/c-brenn/phoenix.vim#installation
Plug 'c-brenn/phoenix.vim' | Plug 'tpope/vim-projectionist'
"Plug 'avdgaag/vim-phoenix' | Plug 'tpope/vim-projectionist'

"-------------------------------------------------------------------------------
" git
"-------------------------------------------------------------------------------

Plug 'airblade/vim-gitgutter'
Plug 'tap349/vim-extradite' | Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb' | Plug 'tpope/vim-fugitive'

"-------------------------------------------------------------------------------
" javascript / coffeescript / react
"-------------------------------------------------------------------------------

"Plug 'kchmck/vim-coffee-script'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx' | Plug 'pangloss/vim-javascript'

"-------------------------------------------------------------------------------
" ruby / rails
"-------------------------------------------------------------------------------

Plug 'keith/rspec.vim'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
" https://github.com/vim-ruby/vim-ruby/issues/32
" commands :A, etc. for ruby non-rails applications,
" also adds lib/ to vim path (same as `set path+=lib`)
Plug 'tpope/vim-rake' | Plug 'tpope/vim-projectionist'

"-------------------------------------------------------------------------------
" other file types support
"-------------------------------------------------------------------------------

" it seemed to me that using vim-polyglot introduced some lag
" in general - when switching tabs, switching to visual mode, etc.
"Plug 'sheerun/vim-polyglot'
Plug 'tap349/vim-markdown'
Plug 'slim-template/vim-slim'
Plug 'slime-lang/vim-slime-syntax'

"-------------------------------------------------------------------------------
" other plugins
"-------------------------------------------------------------------------------

Plug 'ap/vim-css-color'
Plug 'bkad/CamelCaseMotion'
Plug 'easymotion/vim-easymotion'
Plug 'ervandew/supertab'
Plug 'flazz/vim-colorschemes'
Plug 'godlygeek/tabular'
Plug 'itchyny/lightline.vim' | Plug 'tpope/vim-fugitive'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'mhinz/vim-hugefile'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'tap349/ack.vim'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'xolox/vim-misc'
Plug 'yssl/QFEnter'

"-------------------------------------------------------------------------------
" unused (but still can be used again somewhen)
"-------------------------------------------------------------------------------

"Plug 'Yggdroot/indentLine'
"Plug 'jamessan/vim-gnupg'
"Plug 'scheakur/vim-scheakur'
"Plug 'xolox/vim-session'

"-------------------------------------------------------------------------------
" plugins to try in the future
"-------------------------------------------------------------------------------

"Plug 'Shougo/vimfiler.vim'
"Plug 'Shougo/vimshell.vim'

call plug#end()

"===============================================================================
"
" configuration
"
"===============================================================================

"-------------------------------------------------------------------------------
" ack.vim
"
" rg respects ./.gitignore and ~/.ignore files
"-------------------------------------------------------------------------------

let g:ackprg = 'rg --fixed-strings --smart-case --vimgrep'
" disable empty search (searching the word under cursor) -
" it complicates the logic to parse user input excessively
"
" use <C-r><C-w> to paste the word under cursor
let g:ack_use_cword_for_empty_search = 0

" QFEnter works with both quickfix windows and location lists
map <Leader>/ :call <SID>Search()<CR>
map <Leader>\ :call <SID>SearchWithGlob()<CR>

function! s:Search()
  echohl AckSearch
  let l:input_phrase = input(' SEARCH ⮁ ')
  echohl None

  call <SID>MyLAck(l:input_phrase, '')
endfunction

function! s:SearchWithGlob()
  echohl AckSearch
  let l:input_phrase = input(' SEARCH [1/2] ⮁ ')
  redraw!
  let l:glob = input(' GLOB [2/2] ⮁ ')
  echohl None

  call <SID>MyLAck(l:input_phrase, l:glob)
endfunction

" `!` is not allowed in function name
"
" https://github.com/mileszs/ack.vim/issues/5
" https://stackoverflow.com/a/15403852/3632318
" https://stackoverflow.com/questions/5669194
" :help escape()
" :help shellescape()
"
" for rg to work we need:
"
" - not to escape `!` at all
" - to escape `%#` twice
" - to escape other special characters (slashes, etc.) once
" - not to treat strings starting with dashes as rg options
"
" useful functions:
"
" - `shellescape({string})`:
"   escapes all special characters once (excluding `!%#`)
" - `shellescape({string}, 1)`:
"   escapes all special characters once (including `!%#`)
" - `escape({string}, {chars})`:
"   escapes only the characters it's told to escape
" - `--` (options delimiter):
"   signifies the end of rg options
"
" => escape all special characters excluding `!%#` with
"    `shellescape`, escape `%#` with `escape` twice
"    and let `--` deal with strings starting with dashes
function! s:MyLAck(input_phrase, ...)
  let l:glob = get(a:, 1, '')
  let l:glob_option = len(l:glob) ? '-g ''*' . l:glob . '*''' : ''

  let l:delimiter = ' -- '
  let l:split_args = split(a:input_phrase, l:delimiter)
  let l:args_len = len(l:split_args)

  " no arguments
  if l:args_len == 0
    call <SID>ShowWarningMessage('Empty search')
    return
  " options only (`-w -- `)
  elseif l:args_len == 1 && a:input_phrase =~ l:delimiter . '$'
    call <SID>ShowWarningMessage('Empty search')
    return
  " search phrase only (` -- foo` or `foo`)
  elseif l:args_len == 1
    let l:options = l:glob_option
    let l:search_phrase = join(l:split_args)
  " options and search phrase
  else
    let l:options = l:glob_option . ' ' . l:split_args[0]
    let l:search_phrase = join(l:split_args[1:-1], l:delimiter)
  endif

  " ack.vim already escapes `|%#` once in autoload/ack.vim -
  " escape `%#` once again here so that they're escaped twice
  let l:escaped_search_phrase = escape(shellescape(l:search_phrase), '%#')

  " don't use `silent` - it suppresses 'no match found' message
  "
  " search might break if ' -- ' is a substring of search phrase
  " and user doesn't provide options - then part of search phrase
  " is parsed as options which might yield unpredictable results
  exec ':LAck! ' . l:options . l:delimiter . l:escaped_search_phrase
endfunction

function! s:ShowWarningMessage(message)
  redraw!
  echohl WarningMsg
  echo a:message
  echohl None
endfunction

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
" cpsm
"-------------------------------------------------------------------------------

let g:cpsm_match_empty_query = 0

"-------------------------------------------------------------------------------
" ctrlp.vim
"
" wildignore and .gitignore are used to get the list of ignored files
"-------------------------------------------------------------------------------

" instant update causes cursor to appear and flicker
" at the end of last line in match window
let g:ctrlp_lazy_update = 5
let g:ctrlp_map = '<Leader>s'
" default matcher fails to find, say, user model
" when there are a lot of user assets
let g:ctrlp_match_func = { 'match': 'cpsm#CtrlPMatch' }
let g:ctrlp_match_window = 'bottom,order:ttb,max:15'
let g:ctrlp_mruf_relative = 1
let g:ctrlp_root_markers = ['mix.exs']
let g:ctrlp_switch_buffer = 'et'
let g:ctrlp_use_caching = 1
" https://github.com/BurntSushi/ripgrep/issues/75
let g:ctrlp_user_command = 'rg --files %s'
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
let g:lightline.winwidth = 220

" statusline

" https://github.com/itchyny/lightline.vim/issues/223
" it's not possible to hide right arrow separators for NerdTree
" when component returns empty string
let g:lightline.separator = { 'left': '⮀', 'right': '⮂' }
let g:lightline.subseparator = { 'left': '⮁', 'right': '⮃' }

" or else use letters: N, I, R, V, VL, VB, C
let g:lightline.mode_map = {
      \   'n' : 'NORMAL',
      \   'i' : 'INSERT',
      \   'R' : 'REPLACE',
      \   'v' : 'VISUAL',
      \   'V' : 'V-LINE',
      \   "\<C-v>": 'V-BLOCK',
      \   'c' : 'COMMAND'
      \ }

let g:lightline.active = {
      \   'left': [['mode'], ['fugitive', 'ctrlpitem'], ['filename']],
      \   'right': [['syntastic', 'lineinfo'], ['filetype']]
      \ }
let g:lightline.inactive = {
      \   'left': [['filename']],
      \   'right': [['lineinfo'], ['filetype']]
      \ }

" tabline

let g:lightline.tabline = { 'left': [['tabs']], 'right': [] }
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
        \ s:IsTagbar() ? 'Tagbar' :
        \ s:IsNarrowWindow() ? '' : lightline#mode()
endfunction

function! LightlineFugitive()
  if s:IsNotebookWindow() | return '' | end
  if s:IsNarrowWindow() | return '' | end
  if s:IsPluginWindow() | return '' | end

  if !exists('*fugitive#head') | return '' | end

  let l:branch = fugitive#head()
  let l:fname = expand('%')

  if strwidth(l:branch . l:fname) > 0.6 * winwidth(0)
    let l:branch = ''
  elseif l:branch != ''
    let l:branch = '⎇ ' . l:branch
  endif

  return l:branch
endfunction

function! LightlineCtrlPItem()
  if !s:IsCtrlP() | return '' | end

  " :help g:ctrlp_status_func
  " g:lightline.ctrlp_regex: 0 - not regex mode, 1 - regex mode
  call lightline#link('nR'[g:lightline.ctrlp_regex])
  return g:lightline.ctrlp_item
endfunction

" https://github.com/vim-airline/vim-airline/blob/master/autoload/airline/extensions/quickfix.vim
function! LightlineFilename()
  if s:IsPluginWindow() | return '' | end
  if s:IsQuickfix() | return w:quickfix_title | end
  if s:IsExtradite() | return ExtraditeCommitDate() | end

  let l:fname = s:IsNotebookWindow() ? expand('%:t') : expand('%')
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ ('' != l:fname ? l:fname : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineReadonly()
  if s:IsHelp() | return '' | end
  if &ro | return '⭤' | end

  return ''
endfunction

function! LightlineModified()
  return &ma && &mod ? '+' : ''
endfunction

function! LightlineCtrlPMark()
  if !s:IsCtrlP() | return '' | end

  let l:search_modes = [
        \   g:lightline.ctrlp_prev,
        \   g:lightline.ctrlp_item,
        \   g:lightline.ctrlp_next
        \ ]
  return lightline#concatenate(l:search_modes, 0)
endfunction

function! LightlineFiletype()
  if s:IsNotebookWindow() | return '' | end
  if s:IsPluginWindow() | return '' | end

  return &ft != '' ? &ft : 'no ft'
endfunction

function! LightlineLineinfo()
  if s:IsNarrowWindow() | return '' | end
  if s:IsPluginWindow() | return '' | end

  return printf('%3d/%d☰ : %-2d', line('.'), line('$'), col('.'))
endfunction

function! s:IsNarrowWindow()
  return winwidth(0) <= 60
endfunction

function! s:IsNotebookWindow()
  return winwidth(0) <= 90
endfunction

function! s:IsPluginWindow()
  if s:IsNerdTree() | return 1 | end
  if s:IsCtrlP() | return 1 | end
  if s:IsTagbar() | return 1 | end

  return 0
endfunction

function! s:IsNerdTree()
  return expand('%:t') =~ 'NERD_tree'
endfunction

function! s:IsCtrlP()
  " g:lightline.ctrlp_item must be set in ctrlp configuration:
  " has_key(g:lightline, 'ctrlp_item') must return 1
  return expand('%:t') == 'ControlP'
endfunction

function! s:IsTagbar()
  return expand('%:t') =~ 'Tagbar'
endfunction

function! s:IsExtradite()
  return &ft == 'extradite'
endfunction

function! s:IsHelp()
  return &ft =~? 'help'
endfunction

function! s:IsQuickfix()
  return &ft == 'qf'
endfunction

"-------------------------------------------------------------------------------
" nerdcommenter
"
" <C-1>,<C-2>,etc. are not allowed as well as <C-/>. see for details:
" http://vim.1045645.n5.nabble.com/mapping-control-0-1-or-backtick-td1189910.html
"-------------------------------------------------------------------------------

let g:NERDCreateDefaultMappings = 0
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
let g:NERDTreeCascadeSingleChildDir = 0

let g:NERDTreeWinSize = 35

nmap <silent> <F2> :NERDTreeToggle<CR>
nmap <silent> <Leader>n :NERDTreeFind<CR>

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

" turn on debugging (logs to vim messages)
"let g:syntastic_debug = 1
"let g:syntastic_debug = 3
"let g:syntastic_debug = 33

let g:syntastic_javascript_eslint_exe = '$(npm bin)/eslint'
let g:syntastic_ruby_mri_exec = '~/.rbenv/shims/ruby'
let g:syntastic_ruby_rubocop_exec = '~/.rbenv/shims/rubocop'

" profile is defined in cucumber.yml
" uncomment in case you need special options for syntastic
"let g:syntastic_cucumber_cucumber_args='--profile syntastic'

" define available checkers for filetypes
" (by default more checkers might be available).
" all available checkers for all filetypes are listed in
" http://github.com/vim-syntastic/syntastic/blob/master/doc/syntastic-checkers.txt
"
" if this array is empty for some filetype then for that filetype:
" - no checkers are run automatically in active mode
" - no checkers are run when calling SyntasticCheck without arguments
" - checkers can be run by passing explicitly them to SyntasticCheck only
"
" if this array is not empty for some filetype then for that filetype:
" - only specified checkers are run in active mode
" - only specified are run when calling SyntasticCheck without arguments
" - when passing checkers explicitly to SyntasticCheck checkers in this
"   array are ignored
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_ruby_checkers = ['mri', 'rubocop']

" http://vim.wikia.com/wiki/Simplifying_regular_expressions_using_magic_and_no-magic
" '\m^shadowing outer local variable'
let g:syntastic_ruby_mri_quiet_messages = {
      \   'regex': [
      \     '\m`&'' interpreted as argument prefix',
      \     '\m`*'' interpreted as argument prefix',
      \     '\mambiguous first argument; put parentheses or a space even after `/'' operator'
      \   ]
      \ }

" disable automatic checking for all types
" (exceptions can be listed in active_filetypes array)
let g:syntastic_mode_map = {
      \   'mode': 'passive',
      \   'active_filetypes': [],
      \   'passive_filetypes': []
      \ }

" show syntastic errors in separate window
"nmap <silent> <Leader>e :Errors<CR>
nmap <silent> <Leader>cc :call <SID>MySyntasticCheck()<CR>
nmap <silent> <Leader>cr :call <SID>MySyntasticReset()<CR>

" run all checkers from g:syntastic_<filetype>_checkers for
" current filetype unless checkers are passed explicitly as
" arguments to SyntasticCheck
"
" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
function! s:MySyntasticCheck()
  hi ExtraWhitespace guibg=#FFD700 guifg=black
  " adds match for current window only
  call matchadd('ExtraWhitespace', '\s\+$')

  SyntasticCheck
  call lightline#update()
endfunction

function! s:MySyntasticReset()
  call clearmatches()

  SyntasticReset
  call lightline#update()
endfunction

"-------------------------------------------------------------------------------
" tabular
"-------------------------------------------------------------------------------

" see mappings for specific filetypes in ~/.vim/after/ftplugin

"-------------------------------------------------------------------------------
" tagbar
"-------------------------------------------------------------------------------

let g:tagbar_autofocus = 1
let g:tagbar_compact = 0
let g:tagbar_iconchars = ['▸', '▾']
let g:tagbar_silent = 1
let g:tagbar_sort = 0
let g:tagbar_width = 50

let g:tagbar_status_func = 'TagbarStatusFunc'

function! TagbarStatusFunc(current, sort, fname, ...) abort
  return lightline#statusline(!a:current)
endfunction

nmap <silent> <F3> :TagbarToggle<CR>
nmap <silent> <Leader>t :TagbarOpen fj<CR>

"-------------------------------------------------------------------------------
" vim-buffergator
"-------------------------------------------------------------------------------

"let g:buffergator_sort_regime = 'basename'
"let g:buffergator_split_size = 20
let g:buffergator_suppress_keymaps = 1
"let g:buffergator_viewport_split_policy = 'R'
"let g:buffergator_vsplit_size = 60

nmap <silent> <C-p> :BuffergatorMruCyclePrev<CR>
nmap <silent> <C-n> :BuffergatorMruCycleNext<CR>

"-------------------------------------------------------------------------------
" vim-easymotion
"-------------------------------------------------------------------------------

let g:EasyMotion_enter_jump_first = 1

" https://github.com/easymotion/vim-easymotion#default-bindings
map <Leader> <Plug>(easymotion-prefix)

" these are defaults:
"nmap <Leader>f <Plug>(easymotion-f)
"nmap <Leader>t <Plug>(easymotion-f)
"nmap <Leader>F <Plug>(easymotion-F)

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
" vim-jsx
"-------------------------------------------------------------------------------

let g:jsx_ext_required = 0

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

nmap <Leader>, :A<CR>
nmap <Leader>v :AV<CR>

"-------------------------------------------------------------------------------
" vim-session
"-------------------------------------------------------------------------------

"let g:session_autoload = 'no'
"let g:session_autosave = 'no'

"nmap <F7>d :DeleteSession<Space>
"nmap <F7>o :OpenSession<Space>
"nmap <F7>s :SaveSession<Space>
