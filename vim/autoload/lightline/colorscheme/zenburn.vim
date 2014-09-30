" =============================================================================
" Filename: autoload/lightline/colorscheme/zenburn.vim
" Version: 0.0
" Author: acepukas
" License: MIT License
" Last Change: 2013/09/26 12:15:17.
" =============================================================================
let s:base03  = [ '#242424', 235 ]
let s:base023 = [ '#353535', 236 ]
let s:base02  = [ '#444444', 238 ]
let s:base01  = [ '#585858', 240 ]
let s:base00  = [ '#666666', 242 ]
let s:base0   = [ '#808080', 244 ]
let s:base1   = [ '#969696', 247 ]
let s:base2   = [ '#a8a8a8', 248 ]
let s:base3   = [ '#d0d0d0', 252 ]
let s:yellow  = [ '#d7af87', 180 ]
let s:orange  = [ '#ffd7af', 223 ]
let s:red     = [ '#d78787', 174 ]
let s:magenta = [ '#ffaf87', 216 ]
let s:blue    = [ '#5f8787', 66 ]
let s:cyan    = s:blue
let s:green   = [ '#87af87', 108 ]
let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:base03, s:green ], [ s:base3, s:base01 ] ]
let s:p.normal.right = [ [ s:base02, s:base0 ], [ s:base1, s:base01 ] ]
let s:p.inactive.right = [ [ s:base023, s:base01 ], [ s:base00, s:base02 ] ]
let s:p.inactive.left =  [ [ s:base1, s:base02 ], [ s:base00, s:base023 ] ]
let s:p.insert.left = [ [ s:blue, s:base3 ], [ s:base3, s:base01 ] ]
let s:p.replace.left = [ [ s:base03, s:red ], [ s:base3, s:base01 ] ]
let s:p.visual.left = [ [ s:base03, s:orange ], [ s:base3, s:base01 ] ]
let s:p.normal.middle = [ [ s:base2, s:base02 ] ]
let s:p.inactive.middle = [ [ s:base1, s:base023 ] ]
let s:p.tabline.left = [ [ s:base3, s:base00 ] ]
let s:p.tabline.tabsel = [ [ s:base2, s:base023 ] ]
let s:p.tabline.middle = [ [ s:base02, s:base1 ] ]
let s:p.tabline.right = [ [ s:base2, s:base01 ] ]
let s:p.normal.error = [ [ s:base03, s:red ] ]
let s:p.normal.warning = [ [ s:base023, s:yellow ] ]

let g:lightline#colorscheme#zenburn#palette = lightline#colorscheme#flatten(s:p)
