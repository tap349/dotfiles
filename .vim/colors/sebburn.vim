" Vim color file
" Maintainer:  
" Last Change: 
" URL:         
" License:     
"
set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="sebburn"

hi Boolean         guifg=#dca3a3
hi Character       guifg=#dca3a3 gui=bold
hi Comment         guifg=#667f66 gui=italic
hi Conditional     guifg=#ffc8a0 gui=none
hi Constant        guifg=#dca3a3 gui=bold
hi Cursor          guifg=#000d18 guibg=#8faf9f gui=bold
hi CursorColumn    guifg=#dcdccc guibg=#2b2b2b
hi CursorLine      guibg=#303030 gui=bold
hi Debug           guifg=#bca3a3 gui=bold
hi Define          guifg=#ffcfaf gui=bold
hi Delimiter       guifg=#8f8f8f
hi DiffAdd         guifg=#709080 guibg=#313c36 gui=bold
hi DiffChange      guibg=#333333
hi DiffDelete      guifg=#333333 guibg=#464646
hi DiffText        guifg=#ecbcbc guibg=#41363c gui=bold
hi Directory       guifg=#dcdccc gui=bold
hi Error           guifg=#e37170 guibg=#3d3535 gui=none
hi ErrorMsg        guifg=#80d4aa guibg=#2f2f2f gui=bold
hi Exception       guifg=#c3bf9f gui=bold
hi Float           guifg=#c0bed1
hi FoldColumn      guifg=#93b3a3 guibg=#3f4040
hi Folded          guifg=#93b3a3 guibg=#3f4040
hi Function        guifg=#efef8f
hi Identifier      guifg=#f4dcbc
hi Ignore          guifg=#545a4f
hi Include         guifg=#dfaf8f gui=bold
hi IncSearch       guibg=#f8f893 guifg=#385f38
hi Keyword         guifg=#f0dfaf gui=bold
hi Label           guifg=#dfcfaf gui=underline
hi LineNr          guifg=#9c7370 guibg=#1a1a1a
hi Macro           guifg=#ffcfaf gui=bold
hi MatchParen      guibg=#ffc0a0 guifg=#000000 gui=bold
hi ModeMsg         guifg=#ffcfaf gui=none
hi MoreMsg         guifg=#ffffff gui=bold
hi NonText         guifg=#904040 gui=bold
hi Normal          guifg=#ecdccc guibg=#282828
hi Number          guifg=#8cd0d3
hi Operator        guifg=#f0efd0
hi Pmenu           guibg=#242424 guifg=#ccccbc
hi PMenuSel        guibg=#353a37 guifg=#ccdc90 gui=bold
hi PmenuSbar       guibg=#2e3330 guifg=#000000
hi PMenuThumb      guibg=#a0afa0 guifg=#040404
hi PreCondit       guifg=#dfaf8f gui=bold
hi PreProc         guifg=#ffa080 gui=bold
hi Question        guifg=#ffffff gui=bold
hi Repeat          guifg=#ffd7a7 gui=bold
hi Search          guifg=#ffffff guibg=#b07050
hi SignColumn      guifg=#9fafaf guibg=#181818 gui=bold
hi SpecialChar     guifg=#dca3a3 gui=bold
hi SpecialComment  guifg=#82a282 gui=bold
hi Special         guifg=#cfbfaf
hi SpecialKey      guifg=#904040
hi Statement       guifg=#ffc2a0 gui=none
hi StatusLine      guifg=#000000 guibg=#dca630
hi StatusLineNC    guifg=#101010 guibg=#88b090
hi StorageClass    guifg=#c3bf9f gui=bold
hi String          guifg=#8fbfc8
hi Structure       guifg=#efefaf gui=bold
hi TabLineFill     guifg=#cfcfaf guibg=#181818 gui=bold
hi TabLineSel      guifg=#efefef guibg=#1c1c1b gui=bold
hi TabLine         guifg=#b6bf98 guibg=#181818 gui=bold
hi Tag             guifg=#e89393 gui=bold
hi Title           guifg=#efefef gui=bold
hi Todo            guifg=#cfefcc guibg=#000000 gui=bold
hi Typedef         guifg=#dfe4cf gui=bold
hi Type            guifg=#dfdfbf gui=bold
hi Underlined      guifg=#dcdccc gui=underline
hi VertSplit       guifg=#000000 guibg=#000000
hi Visual          guifg=#233323 guibg=#71d3b4 gui=none
hi VisualNOS       guifg=#233323 guibg=#71d3b4 gui=none
hi WarningMsg      guifg=#ffffff guibg=#333333 gui=bold
hi WildMenu        guibg=#2c302d guifg=#cbecd0 gui=underline

hi SpellBad   guisp=#bc6c4c guifg=#dc8c6c
hi SpellCap   guisp=#6c6c9c guifg=#8c8cbc
hi SpellRare  guisp=#bc6c9c guifg=#bc8cbc
hi SpellLocal guisp=#7cac7c guifg=#9ccc9c


if &t_Co > 255
    hi Boolean         ctermfg=181
    hi Character       ctermfg=181   cterm=bold
    hi Comment         ctermfg=108
    hi Conditional     ctermfg=223   cterm=bold
    hi Constant        ctermfg=181   cterm=bold
    hi Cursor          ctermfg=233   ctermbg=109     cterm=bold
    hi Debug           ctermfg=181   cterm=bold
    hi Define          ctermfg=223   cterm=bold
    hi Delimiter       ctermfg=245
    hi DiffAdd         ctermfg=66    ctermbg=237     cterm=bold
    hi DiffChange      ctermbg=236
    hi DiffDelete      ctermfg=236   ctermbg=238
    hi DiffText        ctermfg=217   ctermbg=237     cterm=bold
    hi Directory       ctermfg=188   cterm=bold
    hi Error           ctermfg=210 ctermbg=52 gui=bold
    hi ErrorMsg        ctermfg=115   ctermbg=236     cterm=bold
    hi Exception       ctermfg=249   cterm=bold
    hi Float           ctermfg=251
    hi FoldColumn      ctermfg=109   ctermbg=238
    hi Folded          ctermfg=109   ctermbg=238
    hi Function        ctermfg=228
    hi Identifier      ctermfg=223
    hi IncSearch       ctermbg=228   ctermfg=238
    hi Keyword         ctermfg=223   cterm=bold
    hi Label           ctermfg=187   cterm=underline
    hi LineNr          ctermfg=248   ctermbg=235
    hi Macro           ctermfg=223   cterm=bold
    hi ModeMsg         ctermfg=223   cterm=none
    hi MoreMsg         ctermfg=15    cterm=bold
    hi Number          ctermfg=116
    hi Operator        ctermfg=230
    hi PreCondit       ctermfg=180   cterm=bold
    hi PreProc         ctermfg=223   cterm=bold
    hi Question        ctermfg=15    cterm=bold
    hi Repeat          ctermfg=223   cterm=bold
    hi Search          ctermfg=230   ctermbg=236
    hi SpecialChar     ctermfg=181   cterm=bold
    hi SpecialComment  ctermfg=108   cterm=bold
    hi Special         ctermfg=181
    hi SpecialKey      ctermfg=151
    hi Statement       ctermfg=187   ctermbg=234     cterm=none
    hi StatusLine      ctermfg=236   ctermbg=186
    hi StatusLineNC    ctermfg=235   ctermbg=108
    hi StorageClass    ctermfg=249   cterm=bold
    hi String          ctermfg=174
    hi Structure       ctermfg=229   cterm=bold
    hi Tag             ctermfg=181   cterm=bold
    hi Title           ctermfg=7     ctermbg=234     cterm=bold
    hi Todo            ctermfg=108   ctermbg=234     cterm=bold
    hi Typedef         ctermfg=253   cterm=bold
    hi Type            ctermfg=187   cterm=bold
    hi Underlined      ctermfg=188   ctermbg=234     cterm=bold
    hi VertSplit       ctermfg=236   ctermbg=65
    hi VisualNOS       ctermfg=236   ctermbg=210     cterm=bold
    hi WarningMsg      ctermfg=15    ctermbg=236     cterm=bold
    hi WildMenu        ctermbg=236   ctermfg=194     cterm=bold
    hi CursorLine      ctermbg=236   cterm=none

    hi SpellLocal ctermfg=14  ctermbg=237
    hi SpellBad   ctermfg=9   ctermbg=237
    hi SpellCap   ctermfg=12  ctermbg=237
    hi SpellRare  ctermfg=13  ctermbg=237

    hi PMenu      ctermfg=248  ctermbg=0
    hi PMenuSel   ctermfg=223 ctermbg=235

    hi Normal ctermfg=188 ctermbg=234
    hi NonText         ctermfg=238

endif


