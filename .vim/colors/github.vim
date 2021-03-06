" Vim color file -- with 256 colour support!
"
" Author: Anthony Carapetis <anthony.carapetis@gmail.com>
" Contributors: Lucas Tadeu <lucastadeuteixeira@gmail.com>
"
" Note: Based on github's syntax highlighting theme
"       Used Brian Mock's darkspectrum as a starting point/template
"       Thanks to Ryan Heath for an easy list of some of the colours:
"       http://rpheath.com/posts/356-github-theme-for-syntax-gem

set background=light

if version > 580
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif

let colors_name = "github"

" {{{ General colors
hi Normal   ctermfg=0   ctermbg=255  guifg=#000000   guibg=#F3F3FA
hi Cursor   ctermfg=239   ctermbg=15  guifg=#F8F8FF   guibg=#444454
hi Visual   ctermfg=15   ctermbg=61  guifg=#FFFFFF   guibg=#3465a3
hi VisualNOS   ctermfg=15   ctermbg=24  guifg=#FFFFFF   guibg=#204a87
hi Search   ctermfg=236   ctermbg=228  guifg=#000000   guibg=#FFFF8C  cterm=bold gui=bold
hi Folded   ctermfg=8 ctermbg=15 guifg=#808080 guibg=#E6E6EC gui=none cterm=none
hi Title    ctermfg=167 guifg=#ef5939
hi StatusLine ctermfg=238 ctermbg=250 guifg=#404040 guibg=#bbbbbb gui=bold cterm=bold
hi StatusLineNC ctermfg=238 ctermbg=252 guifg=#404040 guibg=#d4d4d4 gui=italic cterm=italic
hi VertSplit ctermfg=250 ctermbg=250 guibg=#F3F3FA   guifg=#bacfde gui=none cterm=none
hi LineNr   ctermfg=246 ctermbg=15 guifg=#B8B8C2 guibg=#F3F3FA cterm=bold
hi SpecialKey ctermfg=6 guifg=#177F80 gui=italic cterm=italic
hi WarningMsg ctermfg=167 guifg=#df1100
hi ErrorMsg ctermbg=15 ctermfg=196 guibg=#f8f8ff guifg=#ff1100
hi ColorColumn ctermbg=254 guibg=#E8E8EF
hi SignColumn guibg=#F3F3FA
" }}}

" {{{ Vim => 7.0 specific colors
if version >= 700
    hi CursorLine ctermbg=253 guibg=#D8D8DD
    hi MatchParen ctermfg=0 ctermbg=252 guifg=#000000 guibg=#cdcdfd
    "hi Pmenu        ctermfg=15 ctermbg=8 guifg=#ffffff guibg=#808080 gui=bold   cterm=bold
    "hi PmenuSel     ctermfg=0 ctermbg=252 guifg=#000000 guibg=#cdcdfd  gui=italic cterm=italic
    "hi PmenuSbar    ctermfg=238 ctermbg=0 guifg=#444444 guibg=#000000
    "hi PmenuThumb   ctermfg=248 ctermbg=248 guifg=#aaaaaa guibg=#aaaaaa

    " pmenu
    hi Pmenu         guifg=#2d324b   guibg=#e6e8ea
    hi PmenuSel      guifg=#2d324b   guibg=#c6c8d1
    " pmenu scrollbar colors
    hi PmenuSbar     guibg=#c6c8d1
    hi PmenuThumb    guibg=#3d425b
endif
" }}}

" {{{ Diff highlighting
hi DiffAdd    guibg=#BEFECE
hi DiffChange guibg=#DEEEFE
hi DiffText   guibg=#DDDDFF
hi DiffDelete guibg=#FEE2E2
" }}}

" {{{ GitGutter highlighting
hi GitGutterAdd               guibg=#BEFECE guifg=#0E8E0E
hi GitGutterChange            guibg=#DEEEFE guifg=#6E6EFE
hi GitGutterDelete            guibg=#FEE2E2 guifg=#FE4E4E
hi GitGutterChangeDelete      guibg=#FEDEFE guifg=#EE0EEE

hi GitGutterAddLine           guibg=#BEFECE
hi GitGutterChangeLine        guibg=#DEEEFE
hi GitGutterDeleteLine        guibg=#FEE2E2
hi GitGutterChangeDeleteLine  guibg=#FEDEFE
" }}}

" {{{ GitGutter highlighting
hi CommandTHighlightColor guibg=#D7E2EA gui=none
" }}}

" {{{ Syntax highlighting
" TODO: most cterm colors don't match gui colors now
hi Ignore   ctermfg=8 guifg=#808080
hi Identifier   ctermfg=31 guifg=#0086B3
hi PreProc  ctermfg=247 guifg=#A0A0A0 gui=bold cterm=bold
hi Comment  ctermfg=246 guifg=#999988
hi Constant ctermfg=6 guifg=#177F80 gui=none cterm=none
hi String   ctermfg=161 guifg=#D81745
hi Function ctermfg=88 guifg=#990000 gui=bold cterm=bold
hi Statement    ctermfg=0 guifg=#000000 gui=bold cterm=bold
hi Type     ctermfg=60 guifg=#445588 gui=bold   cterm=bold
hi Number   ctermfg=30 guifg=#1C9898
hi Special  ctermfg=28 guifg=#159828 gui=bold   cterm=bold
"hi Todo         ctermbg=15 ctermfg=196 guibg=#f8f8ff guifg=#ff1100 gui=underline cterm=underline
"hi Todo     ctermfg=15 ctermbg=88 guifg=#FFFFFF guibg=#990000 gui=bold cterm=bold
hi Todo         ctermbg=15 ctermfg=196 guibg=#f8f8ff guifg=#ff1100 gui=bold
hi Label        ctermfg=0 guifg=#000000 gui=bold    cterm=bold
hi StorageClass ctermfg=0 guifg=#000000 gui=bold    cterm=bold
hi Structure    ctermfg=0 guifg=#000000 gui=bold    cterm=bold
hi TypeDef      ctermfg=0 guifg=#000000 gui=bold    cterm=bold

" {{{ Links
hi! link FoldColumn Folded
hi! link CursorColumn   CursorLine
hi! link NonText    LineNr
" }}}

" {{{ Aliases
hi link cppSTL          Function
hi link cppSTLType      Type
hi link Character       Number
hi link htmlTag         htmlEndTag
hi link htmlLink        Underlined
hi link pythonFunction  Identifier
hi link Question        Type
hi link CursorIM        Cursor
hi link VisualNOS       Visual
hi link xmlTag          Identifier
hi link xmlTagName      Identifier
hi link shDeref         Identifier
hi link shVariable      Function
hi link rubySharpBang   Special
hi link perlSharpBang   Special
hi link schemeFunc      Statement
" }}}

" {{{ Tabs
hi TabLine ctermfg=238 ctermbg=188 guifg=#404040 guibg=#dddddd gui=none
hi TabLineFill ctermfg=238 ctermbg=188 guifg=#404040 guibg=#dddddd gui=none
hi TabLineSel   ctermfg=238 guifg=#404040 gui=bold
" }}}

" {{{ Spelling
if has("spell")
    hi spellBad     guisp=#fcaf3e
    hi spellCap     guisp=#73d216
    hi spellRare    guisp=#fcaf3e
    hi spellLocal   guisp=#729fcf
endif
" }}}

" {{{ Elixir
"hi elixirPrivateFunctionDeclaration guifg=#114244 gui=bold
hi elixirPrivateFunctionDeclaration guifg=#112244 gui=bold
"hi elixirPrivateFunctionDeclaration guifg=#990000 gui=bold,italic cterm=bold
"hi elixirPrivateFunctionDeclaration guifg=#DD5555 gui=bold
" }}}
