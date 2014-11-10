" ~/.dotfiles/.vim/sessions/feature%2fupdate-progress-bar-on-hover.vim:
" Vim session script.
" Created by session.vim 2.6.4 on 10 November 2014 at 15:54:08.
" Open this file in Vim and run :source % to restore your session.

set guioptions=egm
silent! set guifont=Inconsolata\ LGC:h14
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
badd +3 app/views/ziggurat/wizard/_queries_tutorial.html.slim
badd +7 app/views/ziggurat/wizard/_queries.html.slim
badd +13 app/views/ziggurat/new.html.slim
badd +2 app/views/ziggurat/wizard/_pages.html.slim
badd +3 app/views/ziggurat/wizard/_pages_tutorial.html.slim
badd +92 app/assets/stylesheets/layouts/l-modal.css.sass
badd +15 app/assets/javascripts/pages/sites/wizard/domain.js.coffee
badd +3 app/assets/javascripts/pages/sites/wizard/wizard.js.coffee
badd +0 app/assets/javascripts/pages/ziggurat/wizard/queries.js.coffee
badd +5 app/assets/javascripts/lib/wizard_data.js.coffee
badd +56 app/assets/javascripts/globals/wizard_modal.js.coffee
badd +9 app/assets/javascripts/pages/ziggurat/wizard/wizard.js.coffee
badd +6 app/views/sites/_report.html.slim
badd +64 app/decorators/site/goals_graph_decorator.rb
badd +67 app/services/events_service.rb
badd +12 spec/decorators/site/goals_graph_decorator_spec.rb
badd +16 app/assets/javascripts/pages/sites/show.js.coffee
badd +2 app/assets/javascripts/pages/goals/new.js.coffee
badd +0 app/services/site_progress_generator.rb
badd +29 app/views/sites/show.html.slim
badd +13 app/controllers/sites_controller.rb
badd +22 app/assets/stylesheets/pages/p-sites-show.css.sass
badd +1 app/views/sites/_pending_data.html.slim
badd +1 app/views/sites/_competitor.html.slim
badd +1 app/views/sites/new.html.slim
badd +1 app/views/credentials/ftp.html.slim
badd +88 app/views/credentials/_ftp.html.slim
badd +15 app/views/credentials/cms.html.slim
badd +3 app/views/audits/index.html.slim
badd +1 app/views/audits/_user_statistic.html.slim
badd +1 app/views/audits/_user_behavior.html.slim
badd +17 app/views/ftp/new.html.slim
badd +1 app/views/pages/new.html.slim
badd +80 app/views/pages/index.html.slim
badd +1 app/views/payments/balance.html.slim
badd +0 app/views/payments/new.html.slim
badd +0 spec/factories/statistics_site_histories.rb
badd +21 app/query_objects/advice_stat.rb
badd +12 app/mailers/site_mailer.rb
badd +7 lib/mail_logger_interceptor.rb
badd +27 lib/mailer_common.rb
badd +11 app/models/ui_event.rb
badd +24 app/workers/mixpanel_track.rb
badd +60 app/controllers/application_controller.rb
badd +31 app/workers/ziggurat_generator.rb
badd +27 app/assets/stylesheets/blocks/b-goal-progress.css.sass
badd +28 app/assets/javascripts/pages/audits/show.js.coffee
badd +1 app/assets/javascripts/globals/progress-bar.js.coffee
badd +39 app/assets/javascripts/globals/goal_progress.js.coffee
badd +1 app/models/advices/site_traffic_small_advice.rb
badd +1 app/models/repurchasable_advice.rb
badd +135 app/models/advice.rb
badd +62 app/controllers/audits_controller.rb
badd +78 app/value_objects/site_audits.rb
badd +32 app/views/audits/show.html.slim
badd +14 app/views/audits/_site_traffic.html.slim
badd +37 app/views/audits/_gold_card.html.slim
badd +0 config/routes.rb
badd +0 app/views/sites/_goal_progress.html.slim
badd +19 app/view_objects/pages_view_object.rb
badd +67 app/value_objects/site_audit.rb
badd +11 app/models/advices/site_traffic_advice.rb
badd +1 app/models/services/site_traffic_service.rb
badd +16 app/models/service.rb
badd +24 db/seeds/04_service_costs.rb
badd +1 app/models/advices/site_traffic_large_advice.rb
badd +1 app/models/advices/site_traffic_medium_advice.rb
badd +82 app/decorators/site/audit_decorator.rb
badd +0 spec/decorators/site/audit_decorator_spec.rb
badd +0 app/views/audits/_sale_info.html.slim
badd +389 config/locales/advices.ru.yml
badd +24 Gemfile
badd +1 app/views/ziggurat/index.html.slim
badd +44 app/assets/javascripts/pages/ziggurat/index.js.coffee
badd +44 app/workers/site_tasks_supervisor.rb
badd +171 app/models/site.rb
badd +12 app/services/ziggurat_service.rb
badd +29 app/controllers/ziggurat_controller.rb
badd +0 app/decorators/site_decorator.rb
badd +1 app/services/queries_builder.rb
badd +0 app/assets/javascripts/globals/ga_event.js.coffee
badd +26 app/assets/javascripts/globals/left_menu.js.coffee
badd +1 app/assets/javascripts/globals/locations.js.coffee
badd +1 app/assets/javascripts/globals/main_menu.js.coffee
badd +1 app/assets/javascripts/globals/modal.js.coffee
badd +1 app/assets/javascripts/globals/new_query.js.coffee
badd +1 app/assets/javascripts/globals/preloader.js.coffee
badd +1 app/assets/javascripts/globals/pusher.js.coffee
badd +1 app/assets/javascripts/globals/screenshots.js.coffee
badd +1 app/assets/javascripts/globals/tables.js.coffee
badd +1 app/assets/javascripts/globals/tooltip.js.coffee
badd +1 app/assets/javascripts/globals/top_menu.js.coffee
badd +1 app/assets/javascripts/globals/ui_event.js.coffee
argglobal
silent! argdel *
edit app/views/sites/_report.html.slim
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 115) / 231)
exe 'vert 2resize ' . ((&columns * 199 + 115) / 231)
argglobal
enew
" file NERD_tree_5
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
wincmd w
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 7 - ((6 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
7
normal! 053|
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 115) / 231)
exe 'vert 2resize ' . ((&columns * 199 + 115) / 231)
tabedit app/views/sites/_goal_progress.html.slim
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 8 - ((7 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
8
normal! 0107|
tabedit app/assets/stylesheets/pages/p-sites-show.css.sass
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 22 - ((21 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
22
normal! 015|
tabedit app/views/sites/show.html.slim
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 115) / 231)
exe 'vert 2resize ' . ((&columns * 199 + 115) / 231)
argglobal
enew
" file NERD_tree_7
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
wincmd w
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 29 - ((28 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
29
normal! 024|
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 115) / 231)
exe 'vert 2resize ' . ((&columns * 199 + 115) / 231)
tabedit app/views/audits/_site_traffic.html.slim
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 5 - ((4 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
5
normal! 07|
tabedit app/views/audits/_sale_info.html.slim
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 16 - ((15 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
16
normal! 07|
tabedit app/views/audits/_gold_card.html.slim
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 37 - ((36 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
37
normal! 0102|
tabedit app/assets/javascripts/globals/goal_progress.js.coffee
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 115) / 231)
exe 'vert 2resize ' . ((&columns * 199 + 115) / 231)
argglobal
enew
" file NERD_tree_12
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
wincmd w
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 46 - ((28 * winheight(0) + 27) / 54)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
46
normal! 013|
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 115) / 231)
exe 'vert 2resize ' . ((&columns * 199 + 115) / 231)
tabnext 8
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

1wincmd w
tabnext 1
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
1resize 54|vert 1resize 31|2resize 54|vert 2resize 199|
1wincmd w
tabnext 4
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
1resize 54|vert 1resize 31|2resize 54|vert 2resize 199|
1wincmd w
tabnext 8
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
1resize 54|vert 1resize 31|2resize 54|vert 2resize 199|
2wincmd w
tabnext 8
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
