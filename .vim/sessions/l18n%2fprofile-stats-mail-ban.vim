" ~/.dotfiles/.vim/sessions/l18n%2fprofile-stats-mail-ban.vim:
" Vim session script.
" Created by session.vim 2.13 on 08 November 2015 at 21:38:31.
" Open this file in Vim and run :source % to restore your session.

set guioptions=egm
silent! set guifont=MonacoB\ for\ Powerline:h13
if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'light'
	set background=light
endif
if !exists('g:colors_name') || g:colors_name != 'summerfruit' | colorscheme summerfruit | endif
call setqflist([{'lnum': 36, 'col': 7, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/value_objects/spent_time.rb', 'text': '  def days_part'}, {'lnum': 19, 'col': 21, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/view_objects/spent_time_view.rb', 'text': '      if spent_time.days_part > 0'}, {'lnum': 27, 'col': 44, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/view_objects/spent_time_view.rb', 'text': '        part_texts << part_text(spent_time.days_part, :day)'}, {'lnum': 28, 'col': 20, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/view_objects/spent_time_view.rb', 'text': '      #days = time.days_part > 0 ? " и #{I18n.time_part(time.days_part.to_i, :day)}" : '''''}, {'lnum': 28, 'col': 63, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'app/view_objects/spent_time_view.rb', 'text': '      #days = time.days_part > 0 ? " и #{I18n.time_part(time.days_part.to_i, :day)}" : '''''}, {'lnum': 33, 'col': 10, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'spec/value_objects/spent_time_spec.rb', 'text': '    its(:days_part) { should eq days }'}])
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
badd +128 app/assets/javascripts/lib/jquery.bar.js
badd +33 app/assets/stylesheets/pages/p-user_rates-index.sass
badd +45 app/views/profiles/show.html.slim
badd +0 app/views/profiles/stats.html.slim
badd +6 config/locales/views/profiles.en.yml
badd +10 app/views/profiles/edit/_profile.html.slim
badd +0 app/controllers/profiles_controller.rb
badd +5 app/decorators/user_profile_decorator.rb
badd +6 app/decorators/user_decorator.rb
badd +75 app/view_objects/profile_stats_view.rb
badd +59 app/query_objects/profile_stats_query.rb
badd +44 app/value_objects/spent_time.rb
badd +30 spec/value_objects/spent_time_spec.rb
badd +0 app/query_objects/user_statistics_query.rb
badd +0 app/assets/javascripts/vendor/jquery.tipsy.js
badd +4 lib/i18n_hack.rb
badd +0 app/controllers/application_controller.rb
badd +20 app/view_objects/spent_time_view.rb
badd +2 spec/view_objects/spent_time_view_spec.rb
badd +0 spec/view_objects/versions_view_spec.rb
argglobal
silent! argdel *
set stal=2
edit app/views/profiles/show.html.slim
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
let s:l = 221 - ((17 * winheight(0) + 17) / 34)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
221
normal! 024|
tabedit app/view_objects/profile_stats_view.rb
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
let s:l = 43 - ((8 * winheight(0) + 17) / 34)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
43
normal! 05|
tabedit app/view_objects/spent_time_view.rb
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
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 17) / 34)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
edit spec/view_objects/spent_time_view_spec.rb
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 66 - ((33 * winheight(0) + 17) / 34)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
66
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 79 + 79) / 159)
exe 'vert 2resize ' . ((&columns * 79 + 79) / 159)
tabnext 1
set stal=1
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
