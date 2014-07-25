" ~/.dotfiles/.vim/sessions/features%2fyandex_update.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 18 July 2014 at 15:07:03.
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
badd +134 config/routes.rb
badd +18 app/models/user.rb
badd +136 spec/models/user_spec.rb
badd +26 db/seeds/00_users.rb
badd +2 spec/factories/users.rb
badd +16 config/schedule.rb
badd +1 app/models/query.rb
badd +31 app/models/order.rb
badd +1 Gemfile
badd +1 Capfile
badd +9 app/services/yandex_update_service.rb
badd +10 app/services/service_pays_service.rb
badd +26 app/views/sites/show.html.slim
badd +49 app/views/landing/show.html.slim
badd +0 app/views/devise/mailer/reset_password_instructions.html.slim
badd +1 app/controllers/credentials_controller.rb
badd +1 app/controllers/api/v1/ftp_controller.rb
badd +1 app/exceptions/http_error.rb
badd +9 app/services/api/google/api_base.rb
badd +1 app/services/api/yandex/metrika.rb
badd +1 app/services/api/yandex/api_base.rb
badd +1 config/api/yandex.yml
badd +1 app/services/api/json_api.rb
badd +22 app/services/api/api_base.rb
badd +20 app/services/api/config.rb
badd +1 app/services/api/ahrefs/api_base.rb
badd +1 app/services/api/mlrs/api_base.rb
badd +1 app/services/api/mlrs/seo_text.rb
badd +6 app/services/api/technology/api_base.rb
badd +1 app/services/api/technology/meta_tags_api.rb
badd +1 app/services/api/technology/main_mirror.rb
badd +21 app/services/api/google/analytics.rb
badd +47 app/services/scores_generator.rb
badd +134 app/workers/site_competitors.rb
badd +30 app/models/site.rb
badd +34 app/workers/site_tasks_supervisor.rb
badd +4 app/models/services/mirror_service.rb
badd +11 app/models/yandex_update.rb
badd +4 db/migrate/20140716125228_create_yandex_updates.rb
badd +6 app/models/statistics/site_history.rb
badd +8 spec/models/statistics/site_history_spec.rb
badd +31 spec/models/yandex_update_spec.rb
badd +1 app/workers/complete_order.rb
badd +0 app/workers/fail_worker.rb
badd +1 app/workers/sync_with_rookee.rb
badd +1 app/workers/start_rookee_project.rb
badd +6 ~/dev/uptimus/app/workers/yandex_update_task.rb
badd +1 app/workers/site_domain_age.rb
badd +16 app/workers/query_requisite_links.rb
badd +5 app/controllers/sites_controller.rb
badd +60 app/decorators/site_decorator.rb
badd +35 app/decorators/query_decorator.rb
badd +139 app/value_objects/seo_ziggurat.rb
badd +0 db/schema.rb
badd +10 app/views/payments/_success.html.slim
badd +6 app/views/pages/index.html.slim
badd +12 app/models/advices/page_address_advice.rb
badd +47 app/value_objects/site_audits.rb
badd +1 app/models/advice.rb
badd +113 spec/models/site_spec.rb
badd +5 spec/factories/yandex_updates.rb
badd +0 spec/factories/sites.rb
badd +1 spec/factories/statistics_site_histories.rb
badd +636 Gemfile.lock
badd +273 spec/decorators/site_decorator_spec.rb
badd +12 spec/workers/complete_order_spec.rb
badd +12 spec/workers/yandex_update_task_spec.rb
badd +0 spec/workers/statistics/positions_fetcher_spec.rb
badd +6 spec/services/yandex_update_service_spec.rb
badd +1 spec/services/service_pays_service_spec.rb
badd +6 spec/services/scores_generator_spec.rb
badd +151 spec/vcr_cassettes/yandex_update_service.yml
badd +103 ~/.zshrc
badd +0 spec/spec_helper.rb
badd +35 spec/workers/site_competitors_spec.rb
silent! argdel *
edit config/schedule.rb
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
let s:l = 23 - ((22 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
23
normal! 0
tabedit ~/dev/uptimus/app/workers/yandex_update_task.rb
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
let s:l = 17 - ((16 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
17
normal! 0
wincmd w
" argglobal
edit spec/workers/yandex_update_task_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 23 - ((22 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
23
normal! 044|
wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
tabedit app/services/yandex_update_service.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 87 + 119) / 239)
exe 'vert 3resize ' . ((&columns * 119 + 119) / 239)
" argglobal
enew
" file NERD_tree_1
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
wincmd w
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
let s:l = 7 - ((6 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
7
normal! 022|
wincmd w
" argglobal
edit spec/services/yandex_update_service_spec.rb
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
normal! 020|
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 87 + 119) / 239)
exe 'vert 3resize ' . ((&columns * 119 + 119) / 239)
tabedit spec/spec_helper.rb
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
let s:l = 66 - ((40 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
66
normal! 04|
tabedit app/models/yandex_update.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 122 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 116 + 119) / 239)
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
let s:l = 14 - ((13 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
14
normal! 05|
wincmd w
" argglobal
edit spec/models/yandex_update_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 14 - ((13 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
14
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 122 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 116 + 119) / 239)
tabedit app/decorators/site_decorator.rb
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
let s:l = 56 - ((10 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
56
normal! 024|
wincmd w
" argglobal
edit spec/decorators/site_decorator_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 257 - ((30 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
257
normal! 0
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
tabnext 6
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

tabnext 3
1wincmd w
let s:bufnr_save = bufnr("%")
let s:cwd_save = getcwd()
NERDTree ~/dev/uptimus
if !getbufvar(s:bufnr_save, '&modified')
  let s:wipebuflines = getbufline(s:bufnr_save, 1, '$')
  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))
    silent execute 'bwipeout' s:bufnr_save
  endif
endif
execute "cd" fnameescape(s:cwd_save)
1resize 57|vert 1resize 31|2resize 57|vert 2resize 87|3resize 57|vert 3resize 119|
tabnext 6
2wincmd w
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
