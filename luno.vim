" Vim syntax file
" Language: Luno

if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "luno"

" Keywords
syntax keyword lunoKeyword import if else for while then var fn enum do ret end 
syntax keyword lunoKeyword bool int string any
hi link lunoKeyword Keyword

" Values
syntax region lunoString start=/\v"/ skip=/\v\\./ end=/\v"/
syntax match lunoConst "\vtrue|false|nil"
syntax match lunoConst "\v[0-9]+|0x[0-9a-fA-F]+|0b[01]+"
syntax match lunoConst "\v'(\\.|[^\\'])'"

" Identifiers
syntax match lunoIdent "[a-zA-Z_]+"

hi link lunoString String
hi link lunoConst Constant
hi link lunoIdent Identifier

syntax match lunoComment "\v#.*$"
hi link lunoComment Comment
