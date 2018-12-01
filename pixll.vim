" Vim syntax file
" Language: Pixll
" Maintainer: Mitchell Vitez
" Latest Revision: 30 November 2018

" Keywords
syn keyword pixllKeyword array transform
syn keyword pixllBuiltInTransform rotate darken brighten invert shifthue
syn keyword pixllBuiltInArray colors randomcolors randomcolor black red green blue yellow cyan pink white dark_red dark_green dark_blue dark_yellow dark_cyan purple gray light_red light_green light_blue light_yellow light_pink light_cyan orange salmon aquamarine lime aubergine cerulean

syn match pixllOperator '[-\*]*[@>+*/]\?'
syn match pixllComment "#.*$"

syn match pixllIdentifier "^\w\w*"

syn match pixllNumber '\d\+'
syn match pixllNumber '[-+]\d\+'
syn match pixllNumber '\d\+\.\d*'
syn match pixllNumber '[-+]\d\+\.\d*'

let b:current_syntax = "pixll"
hi def link pixllIdentifier Identifier
hi def link pixllKeyword PreProc
hi def link pixllNumber Constant
hi def link pixllOperator Operator
hi def link pixllBuiltInArray Type
hi def link pixllBuiltInTransform Type
hi def link pixllComment Comment
