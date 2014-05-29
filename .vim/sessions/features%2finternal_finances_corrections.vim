" ~/.vim/sessions/features%2finternal_finances_corrections.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 23 May 2014 at 12:02:13.
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
badd +91 ~/.zshrc
badd +3 app/admin/financial_reports/internal_finances.rb
badd +1 app/models/financial_reports/internal_finance.rb
badd +15 app/models/transaction.rb
badd +7 app/models/correction.rb
badd +17 app/models/site.rb
badd +1 spec/models/site_spec.rb
badd +47 app/controllers/sites_controller.rb
badd +56 spec/controllers/sites_controller_spec.rb
badd +1 app/controllers/queries_controller.rb
badd +17 spec/controllers/queries_controller_spec.rb
badd +6 app/workers/query_traffic.rb
badd +11 spec/factories/sites.rb
badd +11 app/workers/sync_main_mirror.rb
badd +16 spec/workers/sync_main_mirror_spec.rb
badd +1 app/admin/sites.rb
badd +74 spec/models/transaction_spec.rb
badd +6 spec/factories/transactions.rb
badd +1 spec/factories/pages.rb
badd +5 spec/factories/corrections.rb
badd +21 spec/factories/users.rb
badd +1 app/models/user.rb
badd +1 app/models/transaction_details.rb
badd +581 spec/models/financial_reports/internal_finance_spec.rb
badd +1 spec/controllers/admin/financial_reports/internal_finances_controller_spec.rb
badd +18 spec/factories/service_pays.rb
badd +14 app/models/service_pay.rb
badd +17 spec/models/service_pay_spec.rb
badd +8 app/services/service_pays_service.rb
badd +19 app/models/order.rb
badd +2 spec/factories/orders.rb
badd +1 spec/factories/services.rb
badd +8 app/admin/financial_reports/grouped_balances.rb
badd +11 db/seeds.rb
badd +6 db/seeds/00_users.rb
badd +14 db/seeds/scenarios/corrections.rb
badd +43 db/seeds/development.rb
badd +6 lib/tasks/stats.rake
badd +0 lib/tasks/spec.rake
badd +44 app/views/admin/grouped_balances/_internal_finances.builder
badd +58 app/admin/helpers/grouped_report_helper.rb
badd +26 app/workers/collect_balances.rb
badd +1 app/services/balance.rb
badd +467152 log/development.log
silent! argdel *
edit app/admin/financial_reports/internal_finances.rb
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
let s:l = 83 - ((26 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
83
normal! 07|
wincmd w
" argglobal
edit spec/controllers/admin/financial_reports/internal_finances_controller_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 15 - ((14 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
15
normal! 03|
wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
tabedit app/models/financial_reports/internal_finance.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 111 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 127 + 119) / 239)
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
let s:l = 38 - ((37 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
38
normal! 0
wincmd w
" argglobal
edit spec/models/financial_reports/internal_finance_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 581 - ((26 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
581
normal! 050|
wincmd w
exe 'vert 1resize ' . ((&columns * 111 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 127 + 119) / 239)
tabedit app/views/admin/grouped_balances/_internal_finances.builder
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
let s:l = 44 - ((13 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
44
normal! 0
tabedit app/models/transaction.rb
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
let s:l = 3 - ((2 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
3
normal! 021|
wincmd w
" argglobal
edit spec/models/transaction_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 74 - ((28 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
74
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
tabnext 4
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

tabnext 4
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
