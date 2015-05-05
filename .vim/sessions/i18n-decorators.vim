" ~/.dotfiles/.vim/sessions/i18n-decorators.vim:
" Vim session script.
" Created by session.vim 2.10.1 on 29 April 2015 at 22:50:15.
" Open this file in Vim and run :source % to restore your session.

set guioptions=egm
silent! set guifont=Inconsolata\ LGC:h13
if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'dark'
	set background=dark
endif
if !exists('g:colors_name') || g:colors_name != 'ir_black_morr' | colorscheme ir_black_morr | endif
call setqflist([{'lnum': 162, 'col': 15, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/decorators/ani_manga_decorator.rb', 'text': '      date: h.time_ago_in_words(topic.send(order), "%s назад"),'}, {'lnum': 42, 'col': 32, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/decorators/user_decorator.rb', 'text': '      t ''offline'', time_ago: h.time_ago_in_words(last_online_at, nil, true)'}, {'lnum': 6, 'col': 12, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/decorators/user_history_decorator.rb', 'text': '      "#{h.time_ago_in_words updated_at} назад"'}, {'lnum': 20, 'col': 37, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/anime_helper.rb', 'text': '          "было на ТВ %s" % time_ago_in_words(anime.episode_end_at, "%s назад").sub(" #{DateTime.now.year}", '''')'}, {'lnum': 22, 'col': 24, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/anime_helper.rb', 'text': '          str = "%s" % time_ago_in_words(anime.episode_end_at, "%s назад").sub(" #{DateTime.now.year}", '''').sub(''около '', '''')'}, {'lnum': 32, 'col': 15, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/anime_helper.rb', 'text': '        ago = time_ago_in_words(episode_end)'}, {'lnum': 45, 'col': 36, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/anime_helper.rb', 'text': '          "до показа %s" % time_ago_in_words(episode_start).sub(''осталось 1 день'', ''остался 1 день'')'}, {'lnum': 88, 'col': 7, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/application_helper.rb', 'text': '  def time_ago_in_words date, format_string=nil, original=false'}, {'lnum': 90, 'col': 39, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/application_helper.rb', 'text': '      format_string ? format_string % time_ago_in_words(date) : time_ago_in_words(date)'}, {'lnum': 90, 'col': 65, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/helpers/application_helper.rb', 'text': '      format_string ? format_string % time_ago_in_words(date) : time_ago_in_words(date)'}, {'lnum': 55, 'col': 19, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/comments/_comment.html.slim', 'text': '                = time_ago_in_words comment.created_at, "%s назад"'}, {'lnum': 26, 'col': 13, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/dialogs/_dialog.html.slim', 'text': '          = time_ago_in_words dialog.created_at, "%s назад"'}, {'lnum': 17, 'col': 9, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/forum/_news_block.html.slim', 'text': '      = time_ago_in_words entry.created_at, "%s назад"'}, {'lnum': 55, 'col': 19, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/forum/_menu.html.slim', 'text': '                = time_ago_in_words(entry.created_at, "%s назад")'}, {'lnum': 110, 'col': 21, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/forum/_menu.html.slim', 'text': '                  = time_ago_in_words entry[:created_at]'}, {'lnum': 65, 'col': 15, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/messages/_message.html.slim', 'text': '            = time_ago_in_words message.created_at, "%s назад"'}, {'lnum': 34, 'col': 21, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/pages/admin_panel.html.slim', 'text': '                  = time_ago_in_words Time.at(job[''enqueued_at''])'}, {'lnum': 41, 'col': 21, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/pages/admin_panel.html.slim', 'text': '                  = time_ago_in_words Time.at(job[''enqueued_at''])'}, {'lnum': 48, 'col': 21, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/pages/admin_panel.html.slim', 'text': '                  = time_ago_in_words Time.at(job[''enqueued_at''])'}, {'lnum': 23, 'col': 19, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/profiles/show.html.slim', 'text': '                = time_ago_in_words entry[:created_at], "%s назад"'}, {'lnum': 84, 'col': 21, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/users/_menu.html.slim', 'text': '                  = time_ago_in_words entry[:created_at], "%s назад"'}, {'lnum': 18, 'col': 25, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/users/_user.html.slim', 'text': '            span.date = time_ago_in_words entry[:created_at], "%s назад"'}, {'lnum': 22, 'col': 27, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/views/users/_user.html.slim', 'text': '              span.date = time_ago_in_words entry[:created_at], "%s назад"'}])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/dev/shikimori
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +0 app/decorators/ani_manga_decorator/changes_decorator.rb
badd +137 app/controllers/application_controller.rb
badd +16 lib/routing.rb
badd +21 app/controllers/anime_online/anime_videos_controller.rb
badd +0 app/mailers/sendgrid.rb
badd +83 config/routes.rb
badd +51 app/controllers/users_controller.rb
badd +54 app/views/users/index.html.slim
badd +12 config/locales/datetime.ru.yml
badd +1 app/views/users/show.html.slim
badd +1 app/views/users/index.json.jbuilder
badd +12 app/views/users/_user.html.slim
badd +38 app/decorators/user_decorator.rb
badd +1 config/locales/decorators.ru.yml
badd +0 config/locales/decorators.en.yml
badd +162 app/decorators/ani_manga_decorator.rb
badd +32 app/helpers/anime_helper.rb
badd +0 app/helpers/application_helper.rb
badd +151 config/locales/en.yml
badd +245 config/locales/ru.yml
badd +1 config/locales/simple_form.en.yml
badd +1 config/locales/views.yml
argglobal
silent! argdel *
edit app/decorators/ani_manga_decorator/changes_decorator.rb
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 127 + 79) / 159)
argglobal
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
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 1 - ((0 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 127 + 79) / 159)
tabedit app/controllers/application_controller.rb
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 135 - ((20 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
135
normal! 0
tabedit config/routes.rb
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 85 - ((12 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
85
normal! 05|
tabedit app/controllers/users_controller.rb
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 9 - ((8 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
9
normal! 08|
tabedit app/views/users/_user.html.slim
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 31 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 127 + 79) / 159)
argglobal
enew
" file NERD_tree_3
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
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 12 - ((11 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
12
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 127 + 79) / 159)
tabedit app/decorators/user_decorator.rb
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 42 - ((29 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
42
normal! 030|
tabedit app/helpers/application_helper.rb
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 89 - ((21 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
89
normal! 05|
tabedit config/locales/decorators.ru.yml
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 79 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 79 + 79) / 159)
argglobal
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 1 - ((0 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
edit config/locales/decorators.en.yml
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 5 - ((4 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
5
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 79 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 79 + 79) / 159)
tabedit config/locales/en.yml
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
exe 'vert 1resize ' . ((&columns * 31 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 63 + 79) / 159)
exe 'vert 3resize ' . ((&columns * 63 + 79) / 159)
argglobal
enew
" file NERD_tree_4
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
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 231 - ((10 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
231
normal! 0
wincmd w
argglobal
edit config/locales/ru.yml
setlocal fdm=syntax
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
let s:l = 233 - ((10 * winheight(0) + 20) / 41)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
233
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 31 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 63 + 79) / 159)
exe 'vert 3resize ' . ((&columns * 63 + 79) / 159)
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

1wincmd w
tabnext 1
let s:bufnr_save = bufnr("%")
let s:cwd_save = getcwd()
NERDTree ~/dev/shikimori
if !getbufvar(s:bufnr_save, '&modified')
  let s:wipebuflines = getbufline(s:bufnr_save, 1, '$')
  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))
    silent execute 'bwipeout' s:bufnr_save
  endif
endif
execute "cd" fnameescape(s:cwd_save)
1resize 41|vert 1resize 31|2resize 41|vert 2resize 127|
1wincmd w
tabnext 5
let s:bufnr_save = bufnr("%")
let s:cwd_save = getcwd()
NERDTree ~/dev/shikimori
if !getbufvar(s:bufnr_save, '&modified')
  let s:wipebuflines = getbufline(s:bufnr_save, 1, '$')
  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))
    silent execute 'bwipeout' s:bufnr_save
  endif
endif
execute "cd" fnameescape(s:cwd_save)
1resize 41|vert 1resize 31|2resize 41|vert 2resize 127|
1wincmd w
tabnext 9
let s:bufnr_save = bufnr("%")
let s:cwd_save = getcwd()
NERDTree ~/dev/shikimori
if !getbufvar(s:bufnr_save, '&modified')
  let s:wipebuflines = getbufline(s:bufnr_save, 1, '$')
  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))
    silent execute 'bwipeout' s:bufnr_save
  endif
endif
execute "cd" fnameescape(s:cwd_save)
1resize 41|vert 1resize 31|2resize 41|vert 2resize 63|3resize 41|vert 3resize 63|
1wincmd w
tabnext 2
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
