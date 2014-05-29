" ~/.vim/sessions/features%2fmain_mirror_task.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 22 May 2014 at 09:48:43.
" Open this file in Vim and run :source % to restore your session.

set guioptions=egm
silent! set guifont=MonacoB2:h13
if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'light'
	set background=light
endif
if !exists('g:colors_name') || g:colors_name != 'summerfruit' | colorscheme summerfruit | endif
call setqflist([])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/dev/uptimus
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +79 app/models/site.rb
badd +1 app/services/api/technology/main_mirror.rb
badd +1 app/workers/site_competitors.rb
badd +0 app/workers/sync_regions.rb
badd +8 app/workers/query_traffic.rb
badd +38 app/workers/librarian.rb
badd +0 app/workers/keep_balance.rb
badd +9 app/workers/collect_balances.rb
badd +1 app/workers/close_month.rb
badd +40 app/admin/closed_months.rb
badd +0 db/migrate/20140521121343_add_main_mirror_to_sites.rb
badd +9 app/workers/sync_main_mirror.rb
badd +0 app/workers/sync_entries.rb
badd +49 lib/string.rb
badd +31 app/models/page.rb
badd +7 app/decorators/site_decorator.rb
badd +0 spec/models/site_spec.rb
badd +1 spec/workers/site_competitors_spec.rb
badd +9 spec/workers/sync_main_mirror_spec.rb
badd +6 spec/workers/close_month_spec.rb
badd +18 spec/workers/collect_balances_spec.rb
badd +1 spec/workers/create_rookee_advert_spec.rb
badd +3 spec/workers/create_rookee_project_spec.rb
badd +1 spec/workers/destroy_rookee_adverts_spec.rb
badd +1 spec/workers/extended_kernel_spec.rb
badd +1 spec/workers/keep_balance_spec.rb
badd +3 spec/workers/kernel_pages_spec.rb
badd +1 spec/workers/librarian_spec.rb
badd +1 spec/workers/ping_spec.rb
badd +14 spec/workers/query_traffic_spec.rb
badd +24 spec/workers/sync_regions_spec.rb
badd +1 spec/workers/sync_with_rookee_spec.rb
badd +1 spec/workers/stop_rookee_advert_spec.rb
badd +1 spec/workers/start_rookee_project_spec.rb
badd +85 spec/spec_helper.rb
badd +11 spec/factories/sites.rb
badd +9 spec/factories/queries.rb
badd +1 spec/models/query_spec.rb
badd +10 config/initializers/sidekiq.rb
badd +34 app/workers/statistics/base_scheduler.rb
badd +18 app/models/query.rb
badd +1 app/services/api/technology/api_base.rb
badd +34 app/services/api/json_api.rb
badd +14 config/api/technology.yml
badd +0 spec/factories/statistics_query_positions.rb
badd +88 spec/lib/string_spec.rb
silent! argdel *
edit app/services/api/technology/main_mirror.rb
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
" argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 12 - ((11 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
12
normal! 05|
tabedit app/models/site.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
" argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 45 - ((10 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
45
normal! 0
wincmd w
" argglobal
edit spec/models/site_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 39 - ((13 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
39
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
tabedit spec/factories/sites.rb
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
" argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 10 - ((9 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
10
normal! 05|
tabedit app/workers/sync_main_mirror.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
" argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 16 - ((15 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
16
normal! 03|
wincmd w
" argglobal
edit spec/workers/sync_main_mirror_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 20 - ((19 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
20
normal! 054|
wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
tabnext 2
if exists('s:wipebuf')
"   silent exe 'bwipe ' . s:wipebuf
endif
" unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOI
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save

" Support for special windows like quick-fix and plug-in windows.
" Everything down here is generated by vim-session (not supported
" by :mksession out of the box).

tabnext 2
1wincmd w
if exists('s:wipebuf')
  if empty(bufname(s:wipebuf))
if !getbufvar(s:wipebuf, '&modified')
  let s:wipebuflines = getbufline(s:wipebuf, 1, '$')
  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))
    silent execute 'bwipeout' s:wipebuf
  endif
endif
  endif
endif
doautoall SessionLoadPost
unlet SessionLoad
" vim: ft=vim ro nowrap smc=128
