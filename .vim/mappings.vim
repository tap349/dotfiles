"*******************************************************************************
"
" Mappings
"
"*******************************************************************************

" Leader: global and plugin mappings
" LocalLeader: mappings local to current buffer (<buffer> mappings)

let mapleader = ','
let maplocalleader = '\'

" it should allow to use meta key (option/alt) in most mappings
if has('gui_running') | set macmeta | endif

"===============================================================================
"
" All modes
"
"===============================================================================

"-------------------------------------------------------------------------------
" <C-c>, <C-Backspace> and <C-g>
"-------------------------------------------------------------------------------

" turn off highlighting and clear messages,
" <C-c> in normal mode aborts any pending command
nnoremap <silent> <C-c> <C-c>:nohlsearch<Bar>:echo<CR>
nnoremap <silent> <C-g> <C-g>:nohlsearch<Bar>:echo<CR>

" map to <Esc> for vertical editing to work
inoremap <silent> <C-c> <Esc>
inoremap <silent> <C-g> <Esc>

" <C-c> doesn't exit visual and command modes since MacVim 8.1.873 (154)
"
" somehow these mappings do what I want - mapping <C-c>
" to <Esc> doesn't work correctly in command mode
"
" <C-Backspace> and <C-g> in command mode act like <Esc> - they
" interrupt current function execution but don't exit command mode
" immediately like <C-c> (say, when calling user defined vimscript
" function - see s:SearchWithGlob() in plugins.vim)
vnoremap <silent> <C-c> <C-c>
cnoremap <silent> <C-c> <C-c>

vnoremap <silent> <C-g> <C-c>
cnoremap <silent> <C-g> <C-c>

"===============================================================================
"
" Normal mode
"
"===============================================================================

" http://superuser.com/a/382582
" http://vim.wikia.com/wiki/Selecting_your_pasted_text
"
" highlight last inserted or pasted text (works till save)
nnoremap gp `[v`]

" https://stackoverflow.com/a/16987522/3632318
" https://stackoverflow.com/questions/8521846/adding-a-regular-expression-to-vimrc-with-nmap-and-imap
"
" escape `\|` to make regex work in mapping
nnoremap <silent> <Leader>u /[^\d0-\d127]\+\([^\d0-\d127]\\|\s\)\+<CR>

" https://stackoverflow.com/a/24463362/3632318
"
" copy current file path to clipboard
" (path relative to PWD or absolute path if current file is not in PWD)
nnoremap <silent> <Leader>y :let @*=fnamemodify(expand('%'), ':.')<CR>

"-------------------------------------------------------------------------------
" Edit without leaving normal mode
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
" Fold
"-------------------------------------------------------------------------------

nnoremap <Backspace> za

"-------------------------------------------------------------------------------
" Open popular files
"-------------------------------------------------------------------------------

nnoremap <Leader>ov :tabnew<CR>:edit $MYVIMRC<CR>
nnoremap <Leader>oz :tabnew<CR>:edit $ZDOTDIR/.zshenv<CR>

"-------------------------------------------------------------------------------
" Fullscreen
"-------------------------------------------------------------------------------

" doesn't work this way - change toggle fullscreen mode key in
" System Preferences -> Keyboard -> Shortcuts -> App Shortcuts
"macmenu Window.Toggle\ Full\ Screen\ Mode key=<nop>

"-------------------------------------------------------------------------------
" Navigation
"-------------------------------------------------------------------------------

" move vertically by visual line - not real one
" (comment out so far - I don't use line wrapping)
"nnoremap j gj
"nnoremap k gk

" g_ (unlike $) doesn't select newline character in visual mode
nnoremap H ^
nnoremap L g_

" http://vim.1045645.n5.nabble.com/
"   lt-SID-gt-or-s-General-questions-about-functions-td4297515.html#a4298658
"
" use <SID> prefix to call functions defined with 's:' in mapping definition
nnoremap <C-Tab> :call <SID>GoToLastActiveTab()<CR>

" <C-t> is just easier to type (less load for my little finger on Dvorak)
nnoremap <C-t> <C-o>
nnoremap <C-o> <nop>

"------- buffer ----------------------------------------------------------------

" same as <C-6>
nnoremap <silent> <C-s> :b#<CR>
nnoremap <silent> <S-Backspace> :bd<CR>

"nnoremap <silent> <C-p> :bprevious<CR>
"nnoremap <silent> <C-n> :bnext<CR>

"------- tab -------------------------------------------------------------------

nnoremap <silent> <C-Backspace> :tabclose<CR>

" same as using gT and gt
nnoremap <silent> <C-h> :tabprevious<CR>
nnoremap <silent> <C-l> :tabnext<CR>

" these hacks for meta key are not needed when macmeta is set
"nmap <silent> ˙ :tabmove -1<CR>
"nmap <silent> ¬ :tabmove +1<CR>
nmap <silent> <M-h> :tabmove -1<CR>
nmap <silent> <M-l> :tabmove +1<CR>

"------- window ----------------------------------------------------------------

" http://vim.wikia.com/wiki/Open_file_under_cursor
"set isfname-=.
"nnoremap <C-w>F :vertical wincmd f<CR>

" https://vi.stackexchange.com/a/3369
" fallback in case mapping above doesn't work
" (the drawback of this solution is that you get vertical
" split with the same file opened if new file is not found)
nmap <C-w>F <C-w>vgf
"nmap <C-w>F <C-w>f<C-w>L

nnoremap <silent> <S-Up> :resize +5<CR>
nnoremap <silent> <S-Down> :resize -5<CR>
nnoremap <silent> <S-Left> :vertical resize -5<CR>
nnoremap <silent> <S-Right> :vertical resize +5<CR>

"-------------------------------------------------------------------------------
" Reload file using different encoding
"
" :help user-commands
" call function: command! MyFunction call MyFunction()
"-------------------------------------------------------------------------------

command! EncodeInWindows1251 :edit ++encoding=cp1251<CR>
command! EncodeInUTF8 :edit ++encoding=utf-8<CR>

"-------------------------------------------------------------------------------
" Save
"-------------------------------------------------------------------------------

nnoremap <silent> <Tab> :w<CR>

"-------------------------------------------------------------------------------
" Sourcing configuration files
"-------------------------------------------------------------------------------

nnoremap <Leader>.v :source $MYVIMRC<CR>

"-------------------------------------------------------------------------------
" Suppress unwanted keys (set to noop)
"-------------------------------------------------------------------------------

"nnoremap <Up> <nop>
"nnoremap <Down> <nop>
"nnoremap <Left> <nop>
"nnoremap <Right> <nop>

nnoremap Q <nop>
nnoremap K <nop>
nnoremap <F1> <nop>

"-------------------------------------------------------------------------------
" Yank
"-------------------------------------------------------------------------------

nnoremap Y y$

"===============================================================================
"
" Insert mode
"
"===============================================================================

inoremap jj <Esc>

"-------------------------------------------------------------------------------
" Editing
"-------------------------------------------------------------------------------

inoremap <C-d> <Delete>

"-------------------------------------------------------------------------------
" Generation of complementary characters
"-------------------------------------------------------------------------------

inoremap {{ {<Space><Space>}<Esc>hi
inoremap }} {}<Esc>i
"inoremap )) ()<Esc>i
"inoremap ]] []<Esc>i

"-------------------------------------------------------------------------------
" Navigation
"-------------------------------------------------------------------------------

inoremap <C-a> <C-o>I
inoremap <C-b> <Left>
inoremap <C-e> <C-o>A
inoremap <C-f> <Right>
inoremap <C-n> <Down>
inoremap <C-p> <Up>

inoremap <M-b> <S-Left>
inoremap <M-f> <S-Right>

"===============================================================================
"
" Visual mode
"
" http://stackoverflow.com/a/3787802/3632318
"
" vmap and vnoremap work in visual and select modes,
" use xmap and xnoremap to have mapping in visual mode only
"
"===============================================================================

"-------------------------------------------------------------------------------
" Editing
"
" http://stackoverflow.com/a/10723838/3632318
"-------------------------------------------------------------------------------

"xnoremap p "_dP
xnoremap <silent> <C-s> :sort<CR>:w<CR>

"-------------------------------------------------------------------------------
" Navigation
"-------------------------------------------------------------------------------

xnoremap H ^
xnoremap L g_

"-------------------------------------------------------------------------------
" Indenting
"
" http://stackoverflow.com/a/444461
" http://stackoverflow.com/a/1413854
"
" alternatively use:
" . - repeat indenting
" u - undo indenting
" gv - restore last visual block
"-------------------------------------------------------------------------------

"xnoremap < <gv
"xnoremap > >gv

"-------------------------------------------------------------------------------
" Searching
"-------------------------------------------------------------------------------

" IDK what exactly this mapping does but I guess the
" same functionality must be provided by vim-asterisk
"xnoremap * y/<C-r>"<CR>

"===============================================================================
"
" Command mode
"
" https://stackoverflow.com/a/7186983/3632318
" :help cmdline-editing
"
"===============================================================================

cnoremap <C-a> <Home>
cnoremap <C-b> <Left>
cnoremap <C-e> <End>
cnoremap <C-f> <Right>

cnoremap <M-b> <S-Left>
cnoremap <M-f> <S-Right>

"===============================================================================
"
" Functions for mappings
"
"===============================================================================

" finds alternate file using uptech/oss/alt (installed with brew) -
" currently not used because it's not fast enough
"
" define command globally (without s: prefix) so that it can be used
" in after/ftplugin/elixir.vim - I cannot define it in ftplugin file
" because vim complains that it's redefined when opening elixir file
"function! AltCommand(path, vim_command)
"  let l:alternate = system('alt ' . a:path)

"  if empty(l:alternate)
"    echohl WarningMsg
"    echo 'No alternate file for ' . a:path . ' exists!'
"    echohl None
"  else
"    exec a:vim_command . " " . l:alternate
"  endif
"endfunction

function! s:GoToLastActiveTab()
  " return if g:lasttabnr variable is not set - it means current
  " tab is the only one and other tabs have never been opened
  if !exists('g:lasttabnr') | return | endif

  " return if there is only one tab left
  " (all other tabs have been closed)
  if tabpagenr('$') == 1 | return | endif

  exec 'tabnext ' . g:lasttabnr
endfunction
