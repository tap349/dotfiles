"*******************************************************************************
"
" plugins
"
" NOTE: don't use noremap in plugin mappings
"
"*******************************************************************************

"===============================================================================
"
" installation
"
"===============================================================================

call plug#begin('~/.vim/plugged')

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
" javascript / react
"-------------------------------------------------------------------------------

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

Plug 'plasticboy/vim-markdown'
"Plug 'tap349/vim-markdown'
Plug 'slim-template/vim-slim'
Plug 'slime-lang/vim-slime-syntax'

"-------------------------------------------------------------------------------
" linting
"-------------------------------------------------------------------------------

Plug 'maximbaz/lightline-ale' | Plug 'itchyny/lightline.vim'
Plug 'scrooloose/syntastic'
Plug 'w0rp/ale'

"-------------------------------------------------------------------------------
" other plugins
"-------------------------------------------------------------------------------

Plug 'ap/vim-css-color'
Plug 'bkad/CamelCaseMotion'
Plug 'easymotion/vim-easymotion'
Plug 'ervandew/supertab'
Plug 'flazz/vim-colorschemes'
Plug 'godlygeek/tabular'
Plug 'haya14busa/vim-asterisk'
Plug 'itchyny/lightline.vim' | Plug 'tpope/vim-fugitive'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'junegunn/limelight.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'mhinz/vim-hugefile'
Plug 'osyo-manga/vim-anzu'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'tap349/ack.vim'
Plug 'tap349/goyo.vim'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'wincent/command-t', {
      \   'do': 'cd ruby/command-t/ext/command-t && RBENV_VERSION=system ruby extconf.rb && make'
      \ }
Plug 'yssl/QFEnter'

"-------------------------------------------------------------------------------
" unused (but still can be used again somewhen)
"-------------------------------------------------------------------------------

"Plug 'Yggdroot/indentLine'
"Plug 'jamessan/vim-gnupg'
"Plug 'scheakur/vim-scheakur'
"Plug 'xolox/vim-misc'

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
" rg respects .gitignore and .ignore files
"-------------------------------------------------------------------------------

"hi AckSearch guifg=#EF1493 gui=italic,bold
"hi AckSearch guifg=#BF0463 gui=italic
"hi AckSearch guifg=#0D297B gui=italic
"hi AckSearch guifg=#136183 gui=italic,bold
"hi AckSearch guifg=#8B814C gui=italic,bold
"hi AckSearch guifg=#B8860B gui=italic,bold
"hi AckSearch guifg=#68838B gui=italic,bold
"hi AckSearch guifg=#545454 gui=italic
hi AckSearch guifg=#444454 gui=italic

let g:ackprg = 'rg -FS --sort-files --vimgrep'
" disable empty search (searching the word under cursor) -
" it complicates the logic to parse user input excessively
"
" use <C-r><C-w> to paste the word under cursor
let g:ack_use_cword_for_empty_search = 0

" QFEnter works with both quickfix windows and location lists
map <Leader>/ :call <SID>Search()<CR>

" useful symbols: ⎸│⮁⮀
function! s:Search( )
  echohl AckSearch
  let l:input_phrase = input(' SEARCH ⮁ ')
  echohl None

  call <SID>MyLAck(l:input_phrase)
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
"
" NOTE: still IDK how to search for literal '--'
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
  " options and search phrase (`-w -- foo`)
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
" ale
"-------------------------------------------------------------------------------

" NOTE: not applied unless vimrc is sourced
"
" highlight group is not defined in github colorscheme so
" sign column colors might be broken after sourcing vimrc
"hi SignColumn guibg=#F3E4EA

let g:ale_sign_error = '>>'
let g:ale_sign_warning = '>>'

let g:ale_lint_on_enter = 0
" https://github.com/w0rp/ale/issues/505
" issue is closed but ALE still lints opened files
" when g:ale_lint_on_filetype_changed = 1 (default)
let g:ale_lint_on_filetype_changed = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'

let g:ale_linters = {
      \   'javascript': ['eslint', 'flow']
      \ }

nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

"-------------------------------------------------------------------------------
" CamelCaseMotion
"-------------------------------------------------------------------------------

map <silent> w <Plug>CamelCaseMotion_w
map <silent> e <Plug>CamelCaseMotion_e
map <silent> b <Plug>CamelCaseMotion_b

"-------------------------------------------------------------------------------
" command-t
"
" linked version of Ruby: :ruby puts "#{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}"
"
" Command-T respects .gitignore file (only when Git file scanner is used)
" and wildignore option (when g:CommandTWildIgnore is set to &wildignore)
"-------------------------------------------------------------------------------

hi CommandTHighlightColor guibg=#D7E2EA gui=none

let g:CommandTFileScanner = 'git'
" when using git file scanner, new files are not visible by default -
" even after flushing the cache (because they are not tracked by git)
let g:CommandTGitIncludeUntracked = 1
let g:CommandTHighlightColor = 'CommandTHighlightColor'
let g:CommandTMatchWindowAtTop = 0
let g:CommandTMatchWindowReverse = 0
let g:CommandTMaxHeight = 15
let g:CommandTSmartCase = 1
let g:CommandTTraverseSCM = 'pwd'
let g:CommandTWildIgnore = &wildignore

" only tab command will try to find already existing window with
" specified buffer - all other commands will just open a new window
"
" `CommandTOpen tabe` has a bug when file is opened in horizontal
" split in current tab instead of a new tab (in some cases only)
let g:CommandTAcceptSelectionCommand = 'e'
let g:CommandTAcceptSelectionSplitCommand = 'sp'
let g:CommandTAcceptSelectionTabCommand = 'CommandTOpen tabe'
let g:CommandTAcceptSelectionVSplitCommand = 'vs'

let g:CommandTCursorLeftMap = '<C-b>'
let g:CommandTCursorRightMap = '<C-f>'
let g:CommandTRefreshMap = '<C-r>'

nmap <silent> <Leader>n <Plug>(CommandT)
nmap <silent> <Leader>m <Plug>(CommandTMRU)

" `my_` prefix is used when there already exists autocommand group
" with the same name in some vim plugin (or might exist)
"
" when Command-T window is dismissed (cancelled) and there is a
" previous search, all matches of the latter are highlighted for
" a moment (it looks like flickering of previous search matches)
"
" it's a hack to remove this instantaneous highlighting (still it doesn't
" stop the highlighting if it was present before opening Command-T window)
augroup my_command_t
  autocmd!
  autocmd User CommandTDidHideMatchListing nohlsearch
augroup END

" http://vimdoc.sourceforge.net/htmldoc/usr_40.html
" https://github.com/wincent/command-t/pull/315
"command! -nargs=+ GotoOrOpenTab call GotoOrOpenTab('<args>')
"let g:CommandTAcceptSelectionTabCommand = 'GotoOrOpenTab'

"-------------------------------------------------------------------------------
" goyo.vim
"-------------------------------------------------------------------------------

let g:goyo_height = '100%'
let g:goyo_width = 81

function! s:GoyoEnter()
  " cursor line is always vertically centered
  set scrolloff=999

  setlocal colorcolumn=81
  hi ColorColumn guibg=#E8E8EF

  Limelight
endfunction

function! s:GoyoLeave()
  set scrolloff=2

  Limelight!
endfunction

augroup my_goyo
  autocmd!
  autocmd User GoyoEnter call <SID>GoyoEnter()
  autocmd User GoyoLeave call <SID>GoyoLeave()
augroup END

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
      \   'left': [['mode'], ['fugitive'], ['filename']],
      \   'right': [['lineinfo'], ['filetype'], ['linter_errors', 'linter_warnings', 'linter_ok']]
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
      \   'mode': 'MyLightlineMode',
      \   'fugitive': 'MyLightlineFugitive',
      \   'filename': 'MyLightlineFilename',
      \   'filetype': 'MyLightlineFiletype',
      \   'lineinfo': 'MyLightlineLineinfo'
      \ }
" expanding components have priority over function components
" (used for warning and critical components)
"
" expanding components are updated only when lightline#update() is called
" (github.com/itchyny/lightline.vim/blob/master/doc/lightline.txt#L415)
let g:lightline.component_expand = {
      \   'syntastic': 'MySyntasticStatus',
      \   'linter_warnings': 'lightline#ale#warnings',
      \   'linter_errors': 'lightline#ale#errors',
      \   'linter_ok': 'lightline#ale#ok'
      \ }
let g:lightline.component_type = {
      \   'syntastic': 'warning',
      \   'linter_warnings': 'warning',
      \   'linter_errors': 'error'
      \ }

" functions for components
"
" &enc = &encoding
" &fenc = &fileencoding
" &ft = &filetype
" &ma = &modifiable
" &mod = &modified
" &ro = &readonly

function! MyLightlineMode()
  return s:IsNerdTree() ? 'NERD' :
        \ s:IsCommandT() ? 'CommandT' :
        \ s:IsNarrowWindow() ? '' : lightline#mode()
endfunction

function! MyLightlineFugitive()
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

" https://github.com/vim-airline/vim-airline/blob/master/autoload/airline/extensions/quickfix.vim
function! MyLightlineFilename()
  if s:IsExtradite() | return ExtraditeCommitDate() | end
  if s:IsVimPlug() | return expand('%') | end
  if s:IsPluginWindow() | return '' | end
  if s:IsQuickfix() | return w:quickfix_title | end

  let l:fname = s:IsNotebookWindow()
        \ ? expand('%:h:t') . '/' . expand('%:t')
        \ : expand('%')
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

function! MyLightlineFiletype()
  if s:IsNotebookWindow() | return '' | end
  if s:IsPluginWindow() | return '' | end

  return &ft != '' ? &ft : 'no ft'
endfunction

function! MyLightlineLineinfo()
  if s:IsNarrowWindow() | return '' | end
  if s:IsPluginWindow() | return '' | end

  return printf('%3d/%d☰ : %-2d', line('.'), line('$'), col('.'))
endfunction

function! MySyntasticStatus()
  if g:syntastic_mode_map['mode'] == 'passive' | return '' | end

  if s:IsNarrowWindow() | return '' | end
  if s:IsPluginWindow() | return '' | end

  let l:flag = SyntasticStatuslineFlag()
  let l:status = len(l:flag) ? l:flag : 'Syntastic: OK'

  return l:status
endfunction

function! s:IsNarrowWindow()
  return winwidth(0) <= 60
endfunction

function! s:IsNotebookWindow()
  return winwidth(0) <= 90
endfunction

function! s:IsPluginWindow()
  if s:IsNerdTree() | return 1 | end
  if s:IsCommandT() | return 1 | end

  return 0
endfunction

function! s:IsNerdTree()
  return expand('%:t') =~ 'NERD_tree'
endfunction

function! s:IsCommandT()
  return &ft == 'command-t'
endfunction

function! s:IsExtradite()
  return &ft == 'extradite'
endfunction

function! s:IsVimPlug()
  return &ft == 'vim-plug'
endfunction

function! s:IsHelp()
  return &ft =~? 'help'
endfunction

function! s:IsQuickfix()
  return &ft == 'qf'
endfunction

" refresh lightline - or else it might become colorless after sourcing
" vimrc (command was previously called in `augroup vimrc`)
"call lightline#enable()

"-------------------------------------------------------------------------------
" lightline-ale
"-------------------------------------------------------------------------------

let g:lightline#ale#indicator_warnings = 'W:'
let g:lightline#ale#indicator_errors = 'E:'
let g:lightline#ale#indicator_ok = 'OK'

"-------------------------------------------------------------------------------
" limelight.vim
"-------------------------------------------------------------------------------

let g:limelight_default_coefficient = 0.7
let g:limelight_paragraph_span = 0

" settings for markdown
let g:limelight_bop = '^#'
let g:limelight_eop = '\ze\n^#'

let g:limelight_priority = -1

"-------------------------------------------------------------------------------
" nerdcommenter
"
" <C-1>,<C-2>,etc. are not allowed as well as <C-/>. see for details:
" http://vim.1045645.n5.nabble.com/mapping-control-0-1-or-backtick-td1189910.html
"-------------------------------------------------------------------------------

let g:NERDCommentEmptyLines = 1
let g:NERDCreateDefaultMappings = 0
let g:NERDDefaultAlign = 'left'
let g:NERDSpaceDelims = 0

map <Leader><Space> <Plug>NERDCommenterToggle

"-------------------------------------------------------------------------------
" nerdtree
"-------------------------------------------------------------------------------

" fix ugly arrows from Andale Mono MT
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

let g:NERDTreeCaseSensitiveSort = 1
" don't collaps dirs that have only one child
let g:NERDTreeCascadeSingleChildDir = 0

let g:NERDTreeWinSize = 35

nmap <silent> <F2> :NERDTreeToggle<CR>
nmap <silent> <Leader>t :NERDTreeFind<CR>

"-------------------------------------------------------------------------------
" QFEnter
"
" QFEnter respects `switchbuf` option! if selected file is opened
" in another tab, all mappings below just switch to that tab
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
"
" run `SyntasticInfo` to get the list of available
" and enabled checkers for current filetype
"-------------------------------------------------------------------------------

" turn on debugging (logs to Vim messages)
"let g:syntastic_debug = 1
"let g:syntastic_debug = 3
"let g:syntastic_debug = 33

" display errors found by all checkers for current filetype at once
" :help syntastic_aggregate_errors
let g:syntastic_aggregate_errors = 1
let g:syntastic_stl_format = 'Syntax: L%F (%t)'

" passive mode by default
" (automatic checking is disabled for all types -
" exceptions can be listed in active_filetypes)
let g:syntastic_mode_map = {
      \   'mode': 'passive',
      \   'active_filetypes': [],
      \   'passive_filetypes': []
      \ }

let g:syntastic_javascript_eslint_exe = '$(npm bin)/eslint'
" https://github.com/vim-syntastic/syntastic/issues/1754
let g:syntastic_javascript_flow_exe = '$(npm bin)/flow focus-check'
let g:syntastic_ruby_mri_exec = '~/.rbenv/shims/ruby'
let g:syntastic_ruby_rubocop_exec = '~/.rbenv/shims/rubocop'

" http://vim.wikia.com/wiki/Simplifying_regular_expressions_using_magic_and_no-magic
" '\m^shadowing outer local variable'
let g:syntastic_ruby_mri_quiet_messages = {
      \   'regex': [
      \     '\m`&'' interpreted as argument prefix',
      \     '\m`*'' interpreted as argument prefix',
      \     '\mambiguous first argument; put parentheses or a space even after `/'' operator'
      \   ]
      \ }

" profile is defined in cucumber.yml
" uncomment in case you need special options for syntastic
"let g:syntastic_cucumber_cucumber_args='--profile syntastic'

" define available checkers for filetypes explicitly
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
" - only specified checkers are run when calling SyntasticCheck
"   without arguments
" - when passing checkers explicitly to SyntasticCheck, checkers
"   in this array are ignored
let g:syntastic_javascript_checkers = ['eslint', 'flow']
let g:syntastic_ruby_checkers = ['mri', 'rubocop']

nmap <silent> <Leader>c :call <SID>MySyntasticCheck()<CR>

" when mode is active, SyntasticCheck is run on each save
function! s:MySyntasticCheck()
  silent call SyntasticToggleMode()

  if g:syntastic_mode_map['mode'] == 'active'
    augroup my_syntastic
      autocmd!
      autocmd BufWritePost * call lightline#update()
    augroup END

    echo 'Running Syntastic...'
    SyntasticCheck
  else
    augroup my_syntastic
      autocmd!
    augroup END

    " reset syntastic in all buffers (including other tabs) -
    " `SyntasticReset` resets syntastic in current buffer only
    silent call <SID>Bufdo('SyntasticReset')
  endif

  redraw!
  call lightline#update()
endfunction

function! s:Bufdo(command)
  let l:currentBufnr = bufnr('%')
  exec 'bufdo ' . a:command
  exec 'buffer ' . l:currentBufnr
endfunction

" show syntastic errors in separate window
"nmap <silent> <Leader>e :Errors<CR>
"nmap <silent> <Leader>sc :call <SID>MySyntasticCheck()<CR>
"nmap <silent> <Leader>sr :call <SID>MySyntasticReset()<CR>

" run all checkers from g:syntastic_<filetype>_checkers for
" current filetype unless checkers are passed explicitly as
" arguments to SyntasticCheck
"
" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
"function! s:MySyntasticCheck()
"  hi ExtraWhitespace guibg=#FFD700 guifg=black
"  " adds match for current window only
"  call matchadd('ExtraWhitespace', '\s\+$')
"
"  SyntasticCheck
"  call lightline#update()
"endfunction

"function! s:MySyntasticReset()
"  call clearmatches()
"
"  SyntasticReset
"  call lightline#update()
"endfunction

"-------------------------------------------------------------------------------
" tabular
"-------------------------------------------------------------------------------

" see mappings for specific filetypes in ~/.vim/after/ftplugin/

"-------------------------------------------------------------------------------
" vim-anzu
"-------------------------------------------------------------------------------

nmap n <Plug>(anzu-n-with-echo)
nmap N <Plug>(anzu-N-with-echo)
nmap * <Plug>(anzu-star-with-echo)
nmap # <Plug>(anzu-sharp-with-echo)

"-------------------------------------------------------------------------------
" vim-asterisk
"-------------------------------------------------------------------------------

let g:asterisk#keeppos = 1

" the difference between * and g* is that g* never attempts to surround
" searched text with word boundaries - it always searches for substring
map * <Plug>(asterisk-*)
map # <Plug>(asterisk-#)
map g* <Plug>(asterisk-g*)
map g# <Plug>(asterisk-g#)
map z* <Plug>(asterisk-z*)
map gz* <Plug>(asterisk-gz*)
map z# <Plug>(asterisk-z#)
map gz# <Plug>(asterisk-gz#)

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

let g:EasyMotion_do_mapping = 0
let g:EasyMotion_enter_jump_first = 1

" https://github.com/easymotion/vim-easymotion#default-bindings
map <Leader> <Plug>(easymotion-prefix)

" these are defaults (when g:EasyMotion_do_mapping = 1):
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
" vim-flow
"-------------------------------------------------------------------------------

"let g:flow#enable = 0
"" somehow it works without setting correct flowpath
"" (and doesn't work when correct flowpath is set -
"" Flow* commands are not even listed in command line)
""let g:flow#flowpath = '$(npm bin)/flow'
"let g:flow#omnifunc = 0
"
"map <silent> <Leader>fj :FlowJumpToDef<CR>
"map <silent> <Leader>ft :FlowType<CR>

"-------------------------------------------------------------------------------
" vim-fugitive
"-------------------------------------------------------------------------------

nmap <F1> :Gblame<CR>
"nmap <F6> :Gvdiff<CR>

"-------------------------------------------------------------------------------
" vim-gitgutter
"-------------------------------------------------------------------------------

hi GitGutterAdd               guibg=#BEFECE guifg=#0E8E0E
hi GitGutterChange            guibg=#DEEEFE guifg=#6E6EFE
hi GitGutterDelete            guibg=#FEE2E2 guifg=#FE4E4E
hi GitGutterChangeDelete      guibg=#FEDEFE guifg=#EE0EEE

hi GitGutterAddLine           guibg=#BEFECE
hi GitGutterChangeLine        guibg=#DEEEFE
hi GitGutterDeleteLine        guibg=#FEE2E2
hi GitGutterChangeDeleteLine  guibg=#FEDEFE

set updatetime=250

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
"
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
let g:gutentags_enabled = 0
" don't update tag file when any project file is saved -
" run :GutentagsUpdate manually to update tag file
let g:gutentags_generate_on_write = 0
" default project root markers are appended to this list
" (probably gutentags can also use g:ctrlp_root_markers).
"
" don't use .gitignore as project root marker because ~/.gitignore
" might already exist => tags for all files in home directory will
" be created then)
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
" vim-rails
"
" example projections: https://gist.github.com/henrik/5676109
"-------------------------------------------------------------------------------

" suggest creating alternate file if it doesn't exist
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
