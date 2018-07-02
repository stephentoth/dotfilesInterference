syn case ignore

set isk=a-z,A-Z,48-57,',.,_

syn sync fromstart

"Common Z80 Assembly instructions
syn keyword ez80Inst adc add and bit call ccf cp cpd cpdr cpi cpir cpl
syn keyword ez80Inst daa dec di djnz ei ex exx halt im in
syn keyword ez80Inst inc ind ini indr inir jp jr ld ldd lddr ldi lea ldir
syn keyword ez80Inst mlt neg nop or otdr otir out outd outi push pop
syn keyword ez80Inst res ret reti retn rl rla rlc rlca rld
syn keyword ez80Inst rr rra rrc rrca rrd rst sbc scf set sl1 sla slia sll sra
syn keyword ez80Inst srl sub xor

" sjasmplus inst
syn keyword ez80Inst sli exa 

"Grab the condition too
syn match ez80Inst "\s\+jp\s\+n\=[covz]\>" "Match Z C O V NZ NC NO NV
syn match ez80Inst "\s\+jp\s\+p[elo]\=\>" "Match P PE PO PL
syn match ez80Inst "\s\+jp\s\+mi\=\>" "Match M MI
syn match ez80Inst "\s\+jp\s\+eq\>" "Match EQ
syn match ez80Inst "\s\+jp\s\+[gn]e\>" "Match NE GE
syn match ez80Inst "\s\+jp\s\+lt\>" "Match LT
syn match ez80Inst "\s\+jp\s\+sn\=f\>" "Match SF SNF

syn match ez80Inst "\s\+jr\s\+n\=[zc]\>" "Match Z C NZ NC
syn match ez80Inst "\s\+jr\s\+eq\>" "Match EQ
syn match ez80Inst "\s\+jr\s\+[gn]e\>" "Match NE GE
syn match ez80Inst "\s\+jr\s\+lt\>" "Match LT

syn match ez80Inst "\s\+call\s\+n\=[covz]\>" "Match Z C O V NZ NC NO NV
syn match ez80Inst "\s\+call\s\+p[elo]\=\>" "Match P PE PO PL
syn match ez80Inst "\s\+call\s\+mi\=\>" "Match M MI
syn match ez80Inst "\s\+call\s\+eq\>" "Match EQ
syn match ez80Inst "\s\+call\s\+[gn]e\>" "Match NE GE
syn match ez80Inst "\s\+call\s\+lt\>" "Match LT
syn match ez80Inst "\s\+call\s\+sn\=f\>" "Match SF SNF

syn match ez80Inst "\s\+ret\s\+n\=[covz]\>" "Match Z C O V NZ NC NO NV
syn match ez80Inst "\s\+ret\s\+p[elo]\=\>" "Match P PE PO PL
syn match ez80Inst "\s\+ret\s\+mi\=\>" "Match M MI
syn match ez80Inst "\s\+ret\s\+eq\>" "Match EQ
syn match ez80Inst "\s\+ret\s\+[gn]e\>" "Match NE GE
syn match ez80Inst "\s\+ret\s\+lt\>" "Match LT
syn match ez80Inst "\s\+ret\s\+sn\=f\>" "Match SF SNF

" Registers
syn keyword ez80Reg af af' bc de hl ix ixh ixl iy iyh iyl
syn keyword ez80Reg sp a b c d e f h i l r
"sjasmplus
syn keyword ez80Reg lx xl ly yl xh hx yh hy

" Directives
syn keyword ez80PreProc .db .byte .dw .word .end .nolist .assume .org
syn keyword ez80PreProc .equ equ
syn keyword ez80PreProc .echo
syn keyword ez80PreProc include define 
syn keyword ez80PreProc macro endmacro if endif 
"syn keyword ez80PreProc .emptyfill .export .outname .asc .background .bank
"syn keyword ez80PreProc .byt .rombanksize .db .dbcos .dbrnd .dbsin .define .def
"syn keyword ez80PreProc .ds .dsb .dstruct .dsw .dw .dwcos .dwrnd .dwsin .else
"syn keyword ez80PreProc .ende .endif .endm .endme .endr .endro .ends .endst
"syn keyword ez80PreProc .enum .equ .fail .fclose .fopen .fread .fsize .if .ifdef
"syn keyword ez80PreProc .ifdefm .ifeq .ifexists .ifgr .ifgreq .ifle .ifleeq .ifndef
"syn keyword ez80PreProc .ifndefm .ifneq .incbin .incdir .include .input .macro
"syn keyword ez80PreProc .memorymap .org .orga .printt .printv .ramsection .redefine
"syn keyword ez80PreProc .redef .repeat .rept .rombankmap .rombanks .seed .section
"syn keyword ez80PreProc .slot .struct .unbackground .undefine .undef .word
"syn keyword ez80PreProc .shift .sym .symbol

"syn keyword ez80PreProc align data db ds dsb dsw dw fsize instanceof read skip slot swap
"syn keyword ez80PreProc defaultslot slotsize bankstotal banksize banks free force
"syn keyword ez80PreProc semifree size superfree


" sjasmplus directives
"syn keyword ez80PreProc DEVICE ABYTE ABYTEC ABYTEZ ALIGN ASSERT BINARY BLOCK BYTE
"syn keyword ez80PreProc DB DC DD DEFARRAY DEPHASE DEFB DEFD DEFDEVICE DEFINE
"syn keyword ez80PreProc DEFM DEFS DEFW DEVICE DISP DISPLAY DM DS DUP DW DWORD 
"syn keyword ez80PreProc DZ EMPTYTRD ENCODING END ENDLUA ENDMOD ENDT EDUP ENT EQU
"syn keyword ez80PreProc ENDMODULE EXPORT FIELD FPOS INCBIN INCHOB INCLUDE INCLUDELUA 
"syn keyword ez80PreProc INCTRD INSERT LABELSLIST LUA MAP MEMORYMAP MODULE ORG
"syn keyword ez80PreProc OUTPUT PAGE PHASE REPT SAVEBIN SAVEHOB SAVESNA SAVETRD SHELLEXEC 
"syn keyword ez80PreProc SIZE SLOT TEXTAREA UNPHASE WORD IF IFN IFDEF IFNDEF ELSE ENDIF
"syn keyword ez80PreProc MACRO ENDM D24 

" vasm directives
"syn keyword ez80PreProc abyte addr align asc ascii asciiz assert
"syn keyword ez80PreProc binary blk blkw bsz byt byte
"syn keyword ez80PreProc data db dc defb defc defl defp defm defw defs dephase ds dsb dsw dw
"syn keyword ez80PreProc end endif else el endm endmac endmacro endr endrepeat endrep endstruct eq extern even
"syn keyword ez80PreProc fail fill fcb fcc fdb
"syn keyword ez80PreProc global
"syn keyword ez80PreProc if ifdef ifndef ifd ifeq ifne ifgt iflt ifle ifused incbin incdir include
"syn keyword ez80PreProc list local 
"syn keyword ez80PreProc mac macro mdat
"syn keyword ez80PreProc nolist 
"syn keyword ez80PreProc org
"syn keyword ez80PreProc phase
"syn keyword ez80PreProc rept repeat reserve rend rorg
"syn keyword ez80PreProc section set spc string struct structure
"syn keyword ez80PreProc text
"syn keyword ez80PreProc weak wor wrd word 
"syn keyword ez80PreProc xdef xlib xref 

" Strings
syn region ez80String start=/"/ skip=/\\"/ end=/"/ oneline
syn region ez80String start=/'/ end=/'/ oneline

" Labels
syn match ez80Lbl "[A-Z_.?][A-Z_.?0-9]*:\="
syn region ez80Lbl2 start="(" end=")" oneline contains=ez80Number,ez80Lbl,ez80Lbl2,ez80Other
syn match ez80Lbl3 "\<\$\>"

" Operators
syn match ez80Other "[~+\-*/%^&=!<>]"

" Numbers
syn match ez80Number "\<[01]\+b\>"
syn match ez80Number "\<\d\x*h\>"
syn match ez80Number "\<\d\+\>"
syn match ez80Number "\<%[01]\+\>"
syn match ez80Number "\$[0-9a-fA-F]\+"
syn match ez80Number "\<&[01]\+\>"
syn match ez80Number "\<0x[0-9a-fA-F]\+\>"

" Indirect register access
syn region ez80Reg start=/(ix/ end=/)/ keepend oneline contains=ez80Lbl,ez80Number,ez80Reg,ez80Other
syn region ez80Reg start=/(iy/ end=/)/ keepend oneline contains=ez80Lbl,ez80Number,ez80Reg,ez80Other
syn match ez80Reg "(b\=c)"
syn match ez80Reg "(de)"
syn match ez80Reg "(hl)"
syn match ez80Reg "(sp)"

" Todo
syn keyword	cTodo		contained TODO FIXME XXX


" Comments
syn match ez80Comment ";.*$" contains=cTodo
syn match ez80Comment "^\*$"
syn region ez80Comment2 start="\<\.endasm\>" skip="\n" end="\<\.asm\>" keepend contains=ez80Comment,ez80Comment2 
syn region ez80Comment start="\/\*" end="\*\/" contains=cTodo

hi def link cTodo		Todo

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_ez80_syntax_inits")
if version < 508
let did_ez80_syntax_inits = 1
command -nargs=+ HiLink hi link <args>
else
command -nargs=+ HiLink hi def link <args>
endif

HiLink ez80Reg Constant
HiLink ez80Lbl Type
HiLink ez80Lbl2 Type
HiLink ez80Lbl3 Type
HiLink ez80Comment Comment
HiLink ez80Comment2 Comment
HiLink ez80Inst Statement
HiLink ez80Include Include
HiLink ez80PreProc PreProc
HiLink ez80Number Number
HiLink ez80String String
HiLink ez80Other Operator
HiLink ez80Todo Todo

delcommand HiLink
endif

let b:current_syntax = "ez80"
" vim: ts=8

