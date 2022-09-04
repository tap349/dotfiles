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
" Elixir / Phoenix
"-------------------------------------------------------------------------------

Plug 'elixir-lang/vim-elixir'
Plug 'andyl/vim-projectionist-elixir' | Plug 'tpope/vim-projectionist'

"-------------------------------------------------------------------------------
" Git
"-------------------------------------------------------------------------------

Plug 'airblade/vim-gitgutter'
"Plug 'tap349/vim-extradite' | Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb' | Plug 'tpope/vim-fugitive'

"-------------------------------------------------------------------------------
" Javascript / React
"-------------------------------------------------------------------------------

"Plug 'othree/yajs.vim'
Plug 'pangloss/vim-javascript'
"Plug 'amadeus/vim-jsx'
"Plug 'leafgarland/typescript-vim'
"Plug 'peitalin/vim-jsx-typescript'
"Plug 'HerringtonDarkholme/yats.vim'
"Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'

"-------------------------------------------------------------------------------
" CSS / SCSS
"-------------------------------------------------------------------------------

"Plug 'ap/vim-css-color'
" SCSS files are highlighted by default without any plugins but
" sometimes highlighting is getting broken → use dedicated plugin
"Plug 'cakebaker/scss-syntax.vim'

"-------------------------------------------------------------------------------
" Slim / Slime
"-------------------------------------------------------------------------------

"Plug 'slim-template/vim-slim'
"Plug 'slime-lang/vim-slime-syntax'

"-------------------------------------------------------------------------------
" Ruby / Rails
"-------------------------------------------------------------------------------

"Plug 'keith/rspec.vim'
"Plug 'vim-ruby/vim-ruby'
"Plug 'tpope/vim-rails'
" https://github.com/vim-ruby/vim-ruby/issues/32
" commands :A, etc. for ruby non-rails applications,
" also adds lib/ to vim path (same as `set path+=lib`)
"Plug 'tpope/vim-rake' | Plug 'tpope/vim-projectionist'

"-------------------------------------------------------------------------------
" Syntax plugins for other file types
"-------------------------------------------------------------------------------

Plug 'pearofducks/ansible-vim'
Plug 'plasticboy/vim-markdown'

"-------------------------------------------------------------------------------
" Other plugins
"-------------------------------------------------------------------------------

Plug 'bkad/CamelCaseMotion'
Plug 'easymotion/vim-easymotion'
"Plug 'flazz/vim-colorschemes'
Plug 'haya14busa/vim-asterisk'
Plug 'itchyny/lightline.vim' | Plug 'tpope/vim-fugitive'
"Plug 'mhinz/vim-hugefile'
Plug 'mhinz/vim-startify'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'osyo-manga/vim-anzu'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'tap349/QFEnter'
Plug 'tap349/ack.vim'
Plug 'tpope/vim-surround'
" `macvim` formula depends on `ruby` formula
" => Homebrew always installs `ruby` formula
" => asdf, system and Homebrew Ruby versions might differ
" => use Homebrew Ruby to compile Command-T
"
" For macOS Intel:
"     \   'do': 'cd ruby/command-t/ext/command-t && /usr/local/opt/ruby/bin/ruby extconf.rb && make'
Plug 'wincent/command-t', {
      \   'do': 'cd ruby/command-t/ext/command-t && /opt/homebrew/opt/ruby/bin/ruby extconf.rb && make'
      \ }

"-------------------------------------------------------------------------------
" unused (but still can be used again somewhen)
"-------------------------------------------------------------------------------

"Plug 'ervandew/supertab'
"Plug 'godlygeek/tabular'
"Plug 'jamessan/vim-gnupg'
"Plug 'tpope/vim-endwise'
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
hi AckSearch guifg=#444454 gui=italic,bold

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
" ansible-vim
"-------------------------------------------------------------------------------

" normal keywords: import_tasks, when, become, become_user, notify
" extra keywords: register, vars, ignore_errors
"let g:ansible_extra_keywords_highlight = 1

"-------------------------------------------------------------------------------
" CamelCaseMotion
"-------------------------------------------------------------------------------

map <silent> w <Plug>CamelCaseMotion_w
map <silent> e <Plug>CamelCaseMotion_e
map <silent> b <Plug>CamelCaseMotion_b

"-------------------------------------------------------------------------------
" coc.nvim
"
" List of supported language identifiers:
" https://code.visualstudio.com/docs/languages/identifiers#_known-language-identifiers
"
" Some commands like <Plug>(coc-definition) or <Plug>(coc-references) open
" *coc-list-location* when more than 1 result is found
"
" See *coc-list-location* for the list of available actions
" See *coc-list-mapping* for the list of available mappings
" See *coc-list-mapping-custom* for how to customize those mappings
"-------------------------------------------------------------------------------

hi CocErrorSign guifg=#EE0000
hi CocWarningSign guifg=#D26A4D
hi CocInfoSign guifg=#0066CC

nmap <silent> <C-j> <Plug>(coc-diagnostic-prev)
nmap <silent> <C-k> <Plug>(coc-diagnostic-next)

nmap <silent> <C-]> <Plug>(coc-definition)
"nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nmap <Leader>rn <Plug>(coc-rename)

nnoremap <silent> <C-n> :call <SID>show_documentation()<CR>

" https://github.com/neoclide/coc.nvim
function! s:show_documentation()
  if coc#float#has_float()
    " Use <Plug>(coc-float-hide) in mapping
    call coc#float#close_all()
  elseif coc#rpc#ready()
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Disable for coc.nvim custom popup menu to work
"inoremap <silent><expr> <TAB>
"      \ pumvisible() ? "\<C-n>" :
"      \ <SID>check_back_space() ? "\<TAB>" :
"      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~# '\s'
endfunction

" https://github.com/vim/vim/issues/2004#issuecomment-324330465
"inoremap <silent><expr> <CR> pumvisible() && !empty(v:completed_item) ?
"      \ coc#_select_confirm() : "\<CR>"

" https://www.reddit.com/r/neovim/comments/weydql/comment/ikkl1eq
"inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm() : "\<C-y>"
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm() : "\<CR>"

" https://prettier.io/docs/en/vim.html
command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')

"-------------------------------------------------------------------------------
" coc-go
"-------------------------------------------------------------------------------

" https://github.com/josa42/coc-go
augroup coc_go
  autocmd!
  autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')
  autocmd BufWritePre *.go :silent call CocAction('format')
augroup END

"-------------------------------------------------------------------------------
" command-t
"
" linked version of Ruby: :ruby puts "#{RUBY_VERSION}-p#{RUBY_PATCHLEVEL}"
"
" Command-T respects .gitignore file (only when Git file scanner is used)
" and wildignore option (when g:CommandTWildIgnore is set to &wildignore)
"
" start search with `.` to find hidden files
"-------------------------------------------------------------------------------

" NOTE: set CommandTHighlightColor in colorschemes or else this color
"       becomes undefined every time new colorscheme is loaded

"hi CommandTHighlightColor guibg=#D7E2EA gui=none

let g:CommandTFileScanner = 'git'
" when using git file scanner, new files are not visible by default -
" even after flushing the cache (because they are not tracked by git)
let g:CommandTGitIncludeUntracked = 1
let g:CommandTHighlightColor = 'CommandTHighlightColor'
let g:CommandTInputDebounce = 10
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
" lightline.vim
"
" on how statusline of lightline is updated:
" https://github.com/itchyny/lightline.vim/issues/236#issuecomment-315203103
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
"
" UPDATE (2019-10-08): right separators are not shown when not using Core Text
" renderer after upgrading to macOS Catalina => don't use right separators
"let g:lightline.separator = { 'left': '⮀', 'right': '⮂' }
"let g:lightline.subseparator = { 'left': '⮁', 'right': '⮃' }
" UPDATE (2022-09-02): these Unicode characters are not shown in macOS Monterey
" at all => don't use any separators
"let g:lightline.separator = { 'left': '⮀' }
"let g:lightline.subseparator = { 'left': '⮁' }

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

" https://github.com/itchyny/lightline.vim/issues/203
let g:lightline.active = {
      \   'left': [['mode'], ['fugitive'], ['filename']],
      \   'right': [['lineinfo'], ['filetype']]
      \ }
let g:lightline.inactive = {
      \   'left': [['filename']],
      \   'right': [['lineinfo'], ['filetype']]
      \ }

" tabline

let g:lightline.tabline = { 'left': [['tabs']], 'right': [['pwd']] }
let g:lightline.tabline_separator = { 'left': '', 'right': '' }
let g:lightline.tabline_subseparator = { 'left': '', 'right': '' }
let g:lightline.tab = {
      \   'active': ['filename', 'modified'],
      \   'inactive': ['filename', 'modified']
      \ }

" components

" https://stackoverflow.com/a/24463362/3632318
let g:lightline.component = {
      \   'fileencodingformat': '%{&fenc !=# "" ? &fenc : &enc}[%{&ff}]',
      \   'pwd': '%{fnamemodify(getcwd(), ":t")}'
      \ }
" function components are updated on every cursor motion
let g:lightline.component_function = {
      \   'mode': 'MyLightlineMode',
      \   'fugitive': 'MyLightlineFugitive',
      \   'filename': 'MyLightlineFilename',
      \   'filetype': 'MyLightlineFiletype',
      \   'lineinfo': 'MyLightlineLineinfo'
      \ }

" options for components:
"
" &enc = &encoding
" &fenc = &fileencoding
" &ft = &filetype
" &ma = &modifiable
" &mod = &modified
" &ro = &readonly

function! MyLightlineMode()
  return <SID>IsNerdTree() ? 'NERD' :
        \ <SID>IsCommandT() ? 'CommandT' :
        \ <SID>IsNarrowWindow() ? '' : lightline#mode()
endfunction

function! MyLightlineFugitive()
  if <SID>IsNotebookWindow() | return '' | end
  if <SID>IsNarrowWindow() | return '' | end
  if <SID>IsPluginWindow() | return '' | end

  if !exists('*fugitive#head') | return '' | end

  let l:branch = fugitive#head()
  let l:fname = expand('%')

  if strwidth(l:branch . l:fname) > 0.8 * winwidth(0)
    let l:branch = ''
  elseif l:branch != ''
    let l:branch = '⎇ ' . l:branch
  endif

  let l:gitgutter_sign = get(g:, 'gitgutter_enabled', 0) ? ' ±' : ''

  "let l:hunks = ''
  "if get(g:, 'gitgutter_enabled', 0)
  "  let l:hunk_array = GitGutterGetHunkSummary()
  "  let l:hunk_symbols = ['+', '~', '-']
  "
  "  for i in [0, 1, 2]
  "    let l:hunks .= printf('%s%s ', l:hunk_symbols[i], l:hunk_array[i])
  "  endfor
  "endif

  return l:branch . l:gitgutter_sign
endfunction

" https://github.com/vim-airline/vim-airline/blob/master/autoload/airline/extensions/quickfix.vim
function! MyLightlineFilename()
  if <SID>IsExtradite() | return ExtraditeCommitDate() | end
  if <SID>IsVimPlug() | return expand('%') | end
  if <SID>IsPluginWindow() | return '' | end
  if <SID>IsQuickfix() | return w:quickfix_title | end

  " https://stackoverflow.com/a/24463362/3632318
  let l:fname = <SID>IsNotebookWindow()
        \ ? expand('%:h:t') . '/' . expand('%:t')
        \ : fnamemodify(expand('%'), ':~:.')
  return (MyLightlineReadonly() != '' ? MyLightlineReadonly() . ' ' : '') .
        \ (l:fname != '' ? l:fname : '[No Name]') .
        \ (MyLightlineModified() != '' ? ' ' . MyLightlineModified() : '')
endfunction

function! MyLightlineReadonly()
  "if <SID>IsHelp() | return '' | end
  if &ro | return '⭤' | end

  return ''
endfunction

function! MyLightlineModified()
  return &ma && &mod ? '+' : ''
endfunction

function! MyLightlineFiletype()
  if <SID>IsNotebookWindow() | return '' | end
  if <SID>IsPluginWindow() | return '' | end

  return &ft != '' ? &ft : 'no ft'
endfunction

function! MyLightlineLineinfo()
  if <SID>IsNarrowWindow() | return '' | end
  if <SID>IsPluginWindow() | return '' | end

  " col() shows byte index of the column position,
  " virtcol() shows screen column position
  return printf('%3d/%d☰ : %-3d', line('.'), line('$'), virtcol('.'))
endfunction

function! s:IsNarrowWindow()
  return winwidth(0) <= 60
endfunction

function! s:IsNotebookWindow()
  return winwidth(0) <= 90
endfunction

function! s:IsPluginWindow()
  if <SID>IsNerdTree() | return 1 | end
  if <SID>IsCommandT() | return 1 | end

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
let g:qfenter_keymap.open_close = ['<C-CR>']
let g:qfenter_keymap.hopen = ['<C-s>']
let g:qfenter_keymap.vopen = ['<C-v>']
let g:qfenter_keymap.topen = ['<C-t>']

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
" vim-easymotion
"-------------------------------------------------------------------------------

let g:EasyMotion_do_mapping = 0
let g:EasyMotion_enter_jump_first = 1

" https://github.com/easymotion/vim-easymotion#default-bindings
map <Leader> <Plug>(easymotion-prefix)

" these are defaults (when g:EasyMotion_do_mapping = 1):
"nmap <Leader>f <Plug>(easymotion-f)
"nmap <Leader>t <Plug>(easymotion-t)
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

nmap <F1> :Git blame<CR>
"nmap <F6> :Gvdiff<CR>

"-------------------------------------------------------------------------------
" vim-gitgutter
"-------------------------------------------------------------------------------

" NOTE: set custom gitgutter colors in colorschemes or else they're overridden
"       with diff colors from those colorschemes when they are loaded

"hi GitGutterAdd               guibg=#BEFECE guifg=#0E8E0E
"hi GitGutterChange            guibg=#DEEEFE guifg=#6E6EFE
"hi GitGutterDelete            guibg=#FEE2E2 guifg=#FE4E4E
"hi GitGutterChangeDelete      guibg=#FEDEFE guifg=#EE0EEE

"hi GitGutterAddLine           guibg=#BEFECE
"hi GitGutterChangeLine        guibg=#DEEEFE
"hi GitGutterDeleteLine        guibg=#FEE2E2
"hi GitGutterChangeDeleteLine  guibg=#FEDEFE

set updatetime=250

let g:gitgutter_enabled = 0
"let g:gitgutter_map_keys = 0
let g:gitgutter_highlight_lines = 1

nmap <silent> <Leader>ht :GitGutterToggle<CR>
"nmap <silent> <Leader>hs <Plug>GitGutterStageHunk
"nmap <silent> <Leader>hu <Plug>GitGutterUndoHunk

"nmap ]c <Plug>GitGutterNextHunk
"nmap [c <Plug>GitGutterPrevHunk

"nmap <Leader>hp <Plug>GitGutterPreviewHunk
"nmap <Leader>hu <Plug>GitGutterUndoHunk
"nmap <Leader>hs <Plug>GitGutterStageHunk

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
" set to 1 by default (enabled for headers)
let g:vim_markdown_folding_level = 3

let g:vim_markdown_frontmatter = 1

"set conceallevel=2
"let g:vim_markdown_conceal = 1
"let g:vim_markdown_conceal_code_blocks = 0

"-------------------------------------------------------------------------------
" vim-projectionist
"-------------------------------------------------------------------------------

" https://github.com/fatih/vim-go/issues/831#issue-152046633
" https://ahamidi.com/projectionist-for-go/
let g:projectionist_heuristics = {}
let g:projectionist_heuristics['*.go'] = {
      \   '*.go': {
      \       'alternate': '{}_test.go',
      \       'type': 'source'
      \   },
      \   '*_test.go': {
      \       'alternate': '{}.go',
      \       'type': 'test'
      \   }
      \ }

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

"-------------------------------------------------------------------------------
" vim-startify
"-------------------------------------------------------------------------------

let g:startify_change_to_dir = 0
let g:startify_custom_header = []
let g:startify_lists = [
      \ { 'type': 'dir',       'header': ['   MRU ' . getcwd()] },
      \ { 'type': 'files',     'header': ['   MRU']             },
      \ { 'type': 'bookmarks', 'header': ['   Bookmarks']       },
      \ ]
