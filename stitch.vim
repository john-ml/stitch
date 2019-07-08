" Vim syntax file

" Types.
" syn match   stitchType /\<I\d\+\>/
" syn match   stitchType /\<U\d\+\>/
" syn match   stitchType /\<F\d\+\>/

" Keywords.
syn keyword stitchKeyword
          \ type case as if then else
" syn keyword stitchExprKeyword C cons call
syn match stitchSymbol /[.~?&∈,;—@★{}()\[\]:≡=<>+*/\\|-]/
syn match stitchTypeSymbol /[!]/

" syn keyword stitchError TODO

" Misc syntax.
" syn match   stitchNoName /[%@!]\d\+\>/
syn match   stitchNumber /-\?\d\+\([ui]\d\+\)\?/
" syn match   stitchFloat  /-\?\<\d\+\.\d*\(e[+-]\d\+\)\?\>/
" syn match   stitchFloat  /\<0x\x\+\>/
syn keyword stitchBoolean true false
" syn keyword stitchConstant undef null none
syn match   stitchComment /#.*$/
syn region  stitchString start=/"/ skip=/\\"/ end=/"/
" syn match   stitchLabel /[-a-zA-Z$._][-a-zA-Z$._0-9']*:/
syn match   stitchIdentifier /[a-zA-Z_][a-zA-Z_0-9']*/
syn match   stitchCInterpolant /#[a-zA-Z_][a-zA-Z_0-9']*/

syn match   stitchType /\v[uif](8|16|32|64)|bool/

if version >= 508 || !exists("did_c_syn_inits")
  if version < 508
    let did_c_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink stitchType Type
  HiLink stitchTypeSymbol Type
  " HiLink stitchStatement Statement
  HiLink stitchNumber Number
  HiLink stitchComment Comment
  HiLink stitchString String
  " HiLink stitchLabel Label
  HiLink stitchKeyword Keyword
  HiLink stitchExprKeyword Identifier
  HiLink stitchSymbol Keyword
  HiLink stitchBoolean Boolean
  " HiLink stitchFloat Float
  " HiLink stitchNoName Identifier
  " HiLink stitchConstant Constant
  " HiLink stitchSpecialComment SpecialComment
  " HiLink stitchError Error
  HiLink stitchIdentifier Normal
  HiLink stitchCInterpolant Constant

  delcommand HiLink
endif

let b:current_syntax = "stitch"
