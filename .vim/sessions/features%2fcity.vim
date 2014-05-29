" ~/.vim/sessions/features%2fcity.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 13 May 2014 at 11:19:51.
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
badd +33 config/routes.rb
badd +21 app/controllers/sites_controller.rb
badd +356 ~/.vimrc
badd +19 app/assets/javascripts/landing.js.coffee
badd +1 app/controllers/landing_controller.rb
badd +8 app/views/reports/index.html.slim
badd +28 app/views/reports/_no_data.html.slim
badd +70 app/assets/stylesheets/pages/p-reports-index.css.sass
badd +34 app/models/site.rb
badd +12 app/views/settings/city.html.slim
badd +18 app/views/credentials/cms.html.slim
badd +12 app/models/credential.rb
badd +3 app/controllers/credentials_controller.rb
badd +7 app/views/sites/wizard/_region.html.slim
badd +1 app/views/settings/ftp.html.slim
badd +22 app/controllers/settings_controller.rb
badd +6 app/query_objects/regions_query.rb
badd +35 app/models/query.rb
badd +21 app/models/region.rb
badd +1 app/controllers/application_controller.rb
badd +9 app/controllers/users_controller.rb
badd +29 Gemfile
badd +56 app/assets/javascripts/pages/sites/wizard/region.js.coffee
badd +6 app/assets/javascripts/pages/profile.js.coffee
badd +690 app/assets/javascripts/vendor/typeahead.js
badd +7 app/controllers/regions_controller.rb
badd +24 app/views/reports/_query.html.slim
badd +0 app/decorators/site_decorator.rb
badd +32 app/models/ability.rb
badd +19 spec/features/sign_up_spec.rb
badd +23 app/views/reports/_with_data.html.slim
badd +36 app/views/reports/_page.html.slim
badd +1 app/admin/sites.rb
badd +5 app/serializers/region_serializer.rb
badd +2 lib/string.rb
badd +2 app/assets/javascripts/pages/reports.js.coffee
badd +1 app/assets/javascripts/application.js.coffee
badd +1 app/assets/javascripts/core.js.coffee
badd +29 app/assets/javascripts/pages/sites/settings/city.js.coffee
badd +0 app/assets/javascripts/pages/pages/index.js.coffee
badd +95 app/assets/stylesheets/blocks/b-wizard.css.sass
badd +0 lib/autocomplete_input.rb
badd +5 app/assets/stylesheets/blocks/b-site-wizard/region.css.sass
badd +9 ~/dev/uptimus/app/assets/stylesheets/blocks/b-region.css.sass
badd +1 app/views/payments/show.html.slim
badd +2 app/views/sites/edit.html.slim
badd +9 app/views/sites/new.html.slim
badd +5 app/assets/stylesheets/landing.css.sass
badd +1 app/assets/stylesheets/pages/p-sites-show.css.sass
badd +19 app/assets/stylesheets/pages/p-pages-index.css.sass
badd +1 app/assets/stylesheets/application.css.scss
badd +1 app/assets/stylesheets/scaffolds.css.scss
badd +3 app/assets/stylesheets/pages/p-settings-city.css.sass
badd +191 db/schema.rb
badd +1 app/controllers/pages_controller.rb
badd +1 app/controllers/queries_controller.rb
badd +1 app/controllers/reports_controller.rb
badd +1 app/controllers/payments_controller.rb
badd +12 spec/controllers/settings_controller_spec.rb
badd +27 spec/controllers/credentials_controller_spec.rb
badd +79 spec/controllers/sites_controller_spec.rb
badd +6 spec/support/shared_contexts.rb
badd +7 spec/support/shared_examples.rb
badd +1 spec/factories/sites.rb
badd +16 spec/controllers/oauth_tokens_controller_spec.rb
silent! argdel *
edit config/routes.rb
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
let s:l = 40 - ((35 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
40
normal! 0
tabedit app/controllers/regions_controller.rb
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
let s:l = 8 - ((7 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
8
normal! 05|
tabedit app/query_objects/regions_query.rb
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
let s:l = 6 - ((5 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
6
normal! 05|
tabedit app/serializers/region_serializer.rb
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
let s:l = 2 - ((1 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
2
normal! 033|
tabedit ~/dev/uptimus/app/assets/stylesheets/blocks/b-region.css.sass
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 207 + 119) / 239)
" argglobal
enew
" file NERD_tree_22
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
let s:l = 6 - ((5 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
6
normal! 018|
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 207 + 119) / 239)
tabedit app/assets/stylesheets/pages/p-settings-city.css.sass
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
let s:l = 8 - ((7 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
8
normal! 022|
tabedit app/views/settings/city.html.slim
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 207 + 119) / 239)
" argglobal
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
normal! 07|
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 119) / 239)
exe 'vert 2resize ' . ((&columns * 207 + 119) / 239)
tabedit app/assets/javascripts/pages/sites/settings/city.js.coffee
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
let s:l = 21 - ((20 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
21
normal! 028|
tabedit app/controllers/sites_controller.rb
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
let s:l = 28 - ((27 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
28
normal! 0
tabedit app/controllers/settings_controller.rb
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
let s:l = 16 - ((15 * winheight(0) + 28) / 57)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
16
normal! 0
tabnext 10
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

tabnext 5
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
1resize 57|vert 1resize 31|2resize 57|vert 2resize 207|
tabnext 7
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
1resize 57|vert 1resize 31|2resize 57|vert 2resize 207|
tabnext 10
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
