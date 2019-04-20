" =============================================================================
" Filename: autoload/lightline/colorscheme/lucius.vim
" Author: tap349
" License: MIT License
" Last Change: 2017/03/24
" =============================================================================

" http://chir.ag/projects/name-that-color
let s:alto = '#D0D0D0'
let s:athens_gray = '#E6E6EA'
let s:atoll = '#0B6085'
let s:blue = '#0512FB'
let s:ecro_white = '#F4F4E6'
let s:ghost = '#C5CDD8'
let s:link_water = '#DAECF5'
let s:mine_shaft = '#222222'
let s:portafino = '#FFFEB3'
let s:reef = '#B1FEB2'
let s:teal_blue = '#054066'
let s:tundora = '#444444'

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

let s:p.normal.left = [[s:athens_gray, s:atoll], [s:teal_blue, s:link_water]]
let s:p.normal.middle = [[s:tundora, s:athens_gray]]
let s:p.normal.right = copy(s:p.normal.left)
let s:p.normal.warning = [['#0512FB', '#FDE1FD']]
let s:p.normal.error = [['#662529', '#F4DBDC']]
let s:p.normal.ok = [['#004000', '#e5f2e5']]

let s:p.inactive.left = [[s:tundora, s:alto], [s:tundora, s:alto]]
let s:p.inactive.middle = [[s:tundora, s:alto]]
let s:p.inactive.right = copy(s:p.inactive.left)

let s:p.insert.left = [[s:tundora, s:reef], [s:teal_blue, s:link_water]]
let s:p.insert.right = copy(s:p.insert.left)

let s:p.replace.left = [[s:tundora, s:portafino], [s:teal_blue, s:link_water]]
let s:p.replace.right = copy(s:p.replace.left)

let s:p.visual.left = [[s:athens_gray, s:tundora], [s:teal_blue, s:link_water]]
let s:p.visual.right = copy(s:p.visual.left)

let s:p.tabline.left = [[s:tundora, s:athens_gray]]
let s:p.tabline.tabsel = [[s:mine_shaft, s:ghost]]
let s:p.tabline.middle = copy(s:p.tabline.left)
let s:p.tabline.right = [[s:tundora, s:ecro_white]]

let g:lightline#colorscheme#lucius#palette = lightline#colorscheme#fill(s:p)
