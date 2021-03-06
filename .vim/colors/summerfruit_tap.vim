"
" SummerFruit Color Scheme
" ========================
"
" Author:   Armin Ronacher <armin.ronacher@active-4.com>
" Version:  0.1
"
set background=light

hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "summerfruit"

"--------------------------------------------------------------
" Global
"--------------------------------------------------------------

hi Normal       guifg=#000000   guibg=#fefefe
"hi Normal       guifg=#000000   guibg=#fdfdfd
"hi Normal       guifg=#000000   guibg=#e6f1f5

" not editable area and some list chars (precedes, extends)
"hi NonText      guifg=#438ec3   guibg=#a7ccd8
"hi NonText      guifg=#438ec3   guibg=#d7e7f8
hi NonText      guifg=#438ec3   guibg=#f4f4f4
" some list chars (tab, nbsp)
hi SpecialKey   guifg=#336e93   guibg=#f4f4f4

"--------------------------------------------------------------
" Search
"--------------------------------------------------------------

"hi Search       guifg=#000000   guibg=#b9dff3
"hi Search        guifg=#eeeeee    guibg=#135e83
"hi Search       guifg=#404040   guibg=#D7D9F7
hi Search       guifg=#404040   guibg=#ffffb6
"hi Search       guifg=#404040   guibg=#AAEB7F
"hi Search       guifg=#800000   guibg=#9fefb0
"hi Search       guifg=#800000   guibg=#ffae00
hi IncSearch    guifg=#800000   guibg=#ffae00

"--------------------------------------------------------------
" Question
"--------------------------------------------------------------

" help hl-Question
hi Question     guifg=#e50808   guibg=#ffffff   gui=bold

"--------------------------------------------------------------
" Interface Elements
"--------------------------------------------------------------

hi StatusLine   guifg=#cccccc   guibg=#303030
hi StatusLineNC guifg=#cccccc   guibg=#181818
"hi StatusLine   guifg=#ffffff   guibg=#43c464   gui=bold
"hi StatusLineNC guifg=#9bd4a9   guibg=#51b069
hi VertSplit    guibg=#c3dae5   guifg=#fffff0
"hi VertSplit    guibg=#839aa5   guifg=#fffff0
hi Folded       guifg=#3c78a2   guibg=#c3daea
hi IncSearch    guifg=#708090   guibg=#f0e68c
hi Pmenu        guifg=#4d526b   guibg=#e6e8f1
" used for command-t and supertab
hi PmenuSel     guifg=#e6e8f1   guibg=#4d526b
hi PmenuSbar    guibg=#3d425b
hi PmenuThumb   guibg=#c6c8d1
hi SignColumn   guibg=#1b5c8a
hi Cursor       guifg=black     guibg=#edD932
"hi Cursor       guifg=black     guibg=#5FeF00
hi CursorLine   guibg=#e7f7f9
"hi CursorLine   guibg=#e2eff7
hi LineNr       guifg=#149fc0   guibg=#f1fcff
hi CursorLineNr guifg=#034e53   guibg=#d0e8f3   gui=none
"hi CursorLineNr guifg=#eeeeee   guibg=#135e83   gui=bold
"hi MatchParen   guifg=#4A3AAD   guibg=#AABADD
"hi MatchParen   guibg=#FFFF00   guifg=#7A6ADD
hi MatchParen   guibg=#7FFF40   guifg=#135e83
"hi MatchParen   guibg=#cddaf5
hi ColorColumn  guibg=#e7f7f9

"--------------------------------------------------------------
" Specials
"--------------------------------------------------------------

hi Todo         guifg=#e50808   guibg=#dbf3cd   gui=bold
hi Title        guifg=#000000                   gui=none
hi Special      guifg=#e50808
"hi Special      guifg=#fd8900
"hi Visual       guibg=#EFFFDB
hi Visual       guibg=#D8F3FF
"hi Visual       guibg=#C8F9FF
"hi Visual       guibg=#E5EFE9
"hi Visual       guibg=#DFFFD8

"--------------------------------------------------------------
" Syntax Elements
"--------------------------------------------------------------

"hi String       guifg=#0086d2
"hi String       guifg=#0086b2
"hi String       guifg=#0076a2   guibg=#fffde9
hi String       guifg=#0076a2                   gui=none
hi Constant     guifg=#2F3C9C
hi Number       guifg=#0086f7
"hi Number       guifg=#0086f7                   gui=bold
hi Statement    guifg=#1F1C6C                   gui=bold
"hi Statement    guifg=#1F2C7C                   gui=bold
"hi Statement    guifg=#e50808                   gui=bold
"hi Statement    guifg=#2b6cba                   gui=bold
hi Function     guifg=#ee0086                   gui=none
hi PreProc      guifg=#ff0007                   gui=none
"hi Comment      guifg=#abab9b
hi Comment      guifg=#ababab
"hi Comment      guifg=#32b22f   guibg=#fffde9   gui=italic
hi Type         guifg=#2F6C9C                   gui=none
"hi Type         guifg=#70897b                   gui=bold
"hi Type         guifg=#5059EB                   gui=bold
hi Error        guifg=#ffffff   guibg=#d40000
hi Identifier   guifg=#ff0086
"hi Identifier   guifg=#ff0086                   gui=bold
hi Label        guifg=#ff0086

"--------------------------------------------------------------
" HTML Highlighting
"--------------------------------------------------------------

hi htmlTag              guifg=#00bdec           gui=bold
hi htmlEndTag           guifg=#00bdec           gui=bold
hi htmlSpecialTagName   guifg=#4aa04a
hi htmlTagName          guifg=#4aa04a
hi htmlTagN             guifg=#4aa04a

"--------------------------------------------------------------
" Vimdiff (used by git mergetool in particular)
"--------------------------------------------------------------

"hi DiffAdd      guibg=Green   guifg=black
"hi DiffChange   guibg=Yellow  guifg=black
"hi DiffRemove   guibg=Red     guifg=black
hi DiffText     guibg=#ffae00 guifg=black

"--------------------------------------------------------------
" NERDTree
"--------------------------------------------------------------

hi Directory    guifg=#234e83
