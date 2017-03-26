" =============================================================================
" Filename: autoload/lightline/colorscheme/lucius.vim
" Author: tap349
" License: MIT License
" Last Change: 2017/03/24
" =============================================================================

let s:black = '#222222'
let s:blue = '#0512FB'
let s:concrete = '#D0D0D0'
let s:harvest_gold = '#D78700'
let s:link_water_dark = '#C5CDD8'
let s:link_water_light = '#DAECF5'
let s:madang = '#B1FEB2'
let s:mercury = '#E4E4E4'
let s:orient = '#0B6085'
let s:pink_lace = '#FDE1FD'
let s:portafino = '#FFFEB3'
let s:regal_blue = '#054066'
let s:ruddy = '#FC0D1B'
let s:san_marino = '#4472AC'
let s:tuatara = '#444444'
let s:white = '#FFFFFF'
let s:white_smoke = '#F5F5F5'

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}

let s:p.normal.left = [[s:mercury, s:orient], [s:regal_blue, s:link_water_light]]
let s:p.normal.middle = [[s:tuatara, s:mercury]]
let s:p.normal.right = copy(s:p.normal.left)
let s:p.normal.error = [[s:white, s:ruddy]]
let s:p.normal.warning = [[s:blue, s:pink_lace]]

let s:p.inactive.left = [[s:tuatara, s:concrete], [s:tuatara, s:concrete]]
let s:p.inactive.middle = [[s:tuatara, s:concrete]]
let s:p.inactive.right = copy(s:p.inactive.left)

let s:p.insert.left = [[s:tuatara, s:madang], [s:regal_blue, s:link_water_light]]
let s:p.insert.right = copy(s:p.insert.left)

let s:p.replace.left = [[s:tuatara, s:portafino], [s:regal_blue, s:link_water_light]]
let s:p.replace.right = copy(s:p.replace.left)

let s:p.visual.left = [[s:mercury, s:tuatara], [s:regal_blue, s:link_water_light]]
let s:p.visual.right = copy(s:p.visual.left)

let s:p.tabline.left = [[s:tuatara, s:mercury]]
let s:p.tabline.tabsel = [[s:black, s:link_water_dark]]
let s:p.tabline.middle = copy(s:p.tabline.left)
let s:p.tabline.right = copy(s:p.tabline.tabsel)

let g:lightline#colorscheme#lucius#palette = lightline#colorscheme#fill(s:p)
