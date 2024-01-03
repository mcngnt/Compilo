.ORIG #12288
LD R6,CST49
BR IGNORE_CST49
CST49 .FILL #65023
IGNORE_CST49
ADD R5,R6,#0
LD R4,CST51
BR IGNORE_CST51
CST51 .FILL #12948
IGNORE_CST51
LD R3,CST53
BR IGNORE_CST53
CST53 .FILL #12905
IGNORE_CST53
JMP R3
FUN_MULT
ADD R0,R0,#0
BRn MULT_CHANGE_SIGN
BR MULT_INIT
MULT_CHANGE_SIGN
NOT R0,R0
ADD R0,R0,#1
NOT R1,R1
ADD R1,R1,#1
MULT_INIT
AND R2,R2,#0
ADD R2,R2,R0
AND R0,R0,#0
MULT_LOOP
ADD R0,R0,R1
ADD R2,R2,#-1
BRz MULT_STOP
BR MULT_LOOP
MULT_STOP
RET
FUN_DIV
AND R2,R2,#0
ADD R0,R0,#0
BRp DIV_NOT_NEG_B
NOT R0,R0
ADD R0,R0,#1
ADD R2,R2,#1
DIV_NOT_NEG_B
ADD R1,R1,#0
BRp DIV_NOT_NEG_A
NOT R1,R1
ADD R1,R1,#1
ADD R2,R2,#1
DIV_NOT_NEG_A
ST R2,DIV_NEG_V
AND R2,R2,#0
DIV_LOOP
NOT R3,R1
ADD R3,R3,#1
ADD R3,R3,R0
BRp DIV_END_LOOP
AND R3,R3,#0
ADD R3,R3,R0
NOT R3,R3
ADD R3,R3,#1
ADD R1,R1,R3
ADD R2,R2,#1
BR DIV_LOOP
DIV_END_LOOP
ADD R0,R2,#0
LD R2,DIV_NEG_V
ADD R2,R2,#-1
BRz DIV_NEG_RES
BR DIV_END
DIV_NEG_RES
NOT R0,R0
ADD R0,R0,#1
DIV_END
DIV_NEG_V .BLKW #1
RET
FUN_MOD
AND R2,R2,#0
ADD R0,R0,#0
BRp MOD_NOT_NEG_B
NOT R0,R0
ADD R0,R0,#1
MOD_NOT_NEG_B
ADD R1,R1,#0
BRp MOD_NOT_NEG_A
NOT R1,R1
ADD R1,R1,#1
ADD R2,R2,#1
MOD_NOT_NEG_A
MOD_LOOP
NOT R3,R1
ADD R3,R3,#1
ADD R3,R3,R0
BRp MOD_END_LOOP
AND R3,R3,#0
ADD R3,R3,R0
NOT R3,R3
ADD R3,R3,#1
ADD R1,R1,R3
BR MOD_LOOP
MOD_END_LOOP
ADD R0,R1,#0
ADD R2,R2,#0
BRp MOD_NEG_RES
BR MOD_END
MOD_NEG_RES
NOT R0,R0
ADD R0,R0,#1
MOD_END
RET
FUN_USER_print_int
STR R5,R6,#0
ADD R6,R6,#-1
ADD R1,R6,#1
STR R1,R6,#0
ADD R6,R6,#-1
STR R7,R6,#0
ADD R6,R6,#-1
ADD R5,R6,#0
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
AND R0,R0,#0
ADD R0,R0,#9
ADD R6,R6,#1
LDR R1,R6,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
ADD R0,R0,#0
BRnz IGNORE_JMP_IF6
LD R3,CST55
BR IGNORE_CST55
CST55 .FILL #12410
IGNORE_CST55
JMP R3
IGNORE_JMP_IF6
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST57
BR IGNORE_CST57
CST57 .FILL #12411
IGNORE_CST57
JMP R3
CMP_ELSE6
AND R0,R0,#0
CMP_ENDELSE6
ADD R0,R0,#0
BRnp IGNORE_JMP_IF9
LD R3,CST59
BR IGNORE_CST59
CST59 .FILL #12442
IGNORE_CST59
JMP R3
IGNORE_JMP_IF9
LD R0,CST7
BR IGNORE_CST7
CST7 .FILL #48
IGNORE_CST7
STR R0,R6,#0
ADD R6,R6,#-1
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
ADD R6,R6,#1
LDR R1,R6,#0
ADD R0,R0,R1
OUT
AND R0,R0,#0
LDR R7,R5,#1
LDR R6,R5,#2
LDR R5,R5,#3
RET
LD R3,CST61
BR IGNORE_CST61
CST61 .FILL #12442
IGNORE_CST61
JMP R3
IF_ELSE9
IF_ENDELSE9
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
AND R0,R0,#0
ADD R0,R0,#10
ADD R6,R6,#1
LDR R1,R6,#0
LD R3,CST63
BR IGNORE_CST63
CST63 .FILL #12314
IGNORE_CST63
JSRR R3
STR R0,R6,#0
ADD R6,R6,#-1
LD R3,CST65
BR IGNORE_CST65
CST65 .FILL #12374
IGNORE_CST65
JSRR R3
AND R1,R1,#0
ADD R1,R1,#1
ADD R6,R6,R1
LD R0,CST1
BR IGNORE_CST1
CST1 .FILL #48
IGNORE_CST1
STR R0,R6,#0
ADD R6,R6,#-1
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
AND R0,R0,#0
ADD R0,R0,#10
ADD R6,R6,#1
LDR R1,R6,#0
LD R3,CST67
BR IGNORE_CST67
CST67 .FILL #12347
IGNORE_CST67
JSRR R3
ADD R6,R6,#1
LDR R1,R6,#0
ADD R0,R0,R1
OUT
AND R0,R0,#0
LDR R7,R5,#1
LDR R6,R5,#2
LDR R5,R5,#3
RET
FUN_USER_print_int_nl
STR R5,R6,#0
ADD R6,R6,#-1
ADD R1,R6,#1
STR R1,R6,#0
ADD R6,R6,#-1
STR R7,R6,#0
ADD R6,R6,#-1
ADD R5,R6,#0
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
AND R0,R0,#0
ADD R0,R0,#0
ADD R6,R6,#1
LDR R1,R6,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
ADD R0,R0,#0
BRn IGNORE_JMP_IF13
LD R3,CST69
BR IGNORE_CST69
CST69 .FILL #12535
IGNORE_CST69
JMP R3
IGNORE_JMP_IF13
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST71
BR IGNORE_CST71
CST71 .FILL #12536
IGNORE_CST71
JMP R3
CMP_ELSE13
AND R0,R0,#0
CMP_ENDELSE13
ADD R0,R0,#0
BRnp IGNORE_JMP_IF17
LD R3,CST73
BR IGNORE_CST73
CST73 .FILL #12565
IGNORE_CST73
JMP R3
IGNORE_JMP_IF17
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
NOT R0,R0
ADD R0,R0,#1
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
STR R0,R1,#0
LD R0,CST14
BR IGNORE_CST14
CST14 .FILL #45
IGNORE_CST14
OUT
LD R3,CST75
BR IGNORE_CST75
CST75 .FILL #12565
IGNORE_CST75
JMP R3
IF_ELSE17
IF_ENDELSE17
AND R1,R1,#0
ADD R1,R1,#-4
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
LD R3,CST77
BR IGNORE_CST77
CST77 .FILL #12374
IGNORE_CST77
JSRR R3
AND R1,R1,#0
ADD R1,R1,#1
ADD R6,R6,R1
AND R0,R0,#0
ADD R0,R0,#10
OUT
AND R0,R0,#0
LDR R7,R5,#1
LDR R6,R5,#2
LDR R5,R5,#3
RET
FUN_USER_f
STR R5,R6,#0
ADD R6,R6,#-1
ADD R1,R6,#1
STR R1,R6,#0
ADD R6,R6,#-1
STR R7,R6,#0
ADD R6,R6,#-1
ADD R5,R6,#0
ADD R6,R6,#-1
ADD R6,R6,#-1
LD R0,CST43
BR IGNORE_CST43
CST43 .FILL #100
IGNORE_CST43
NOT R0,R0
ADD R0,R0,#1
AND R1,R1,#0
ADD R1,R1,#1
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
STR R0,R1,#0
STARTWHILE17
AND R1,R1,#0
ADD R1,R1,#1
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
LD R0,CST20
BR IGNORE_CST20
CST20 .FILL #100
IGNORE_CST20
ADD R6,R6,#1
LDR R1,R6,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
ADD R0,R0,#0
BRn IGNORE_JMP_IF21
LD R3,CST79
BR IGNORE_CST79
CST79 .FILL #12639
IGNORE_CST79
JMP R3
IGNORE_JMP_IF21
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST81
BR IGNORE_CST81
CST81 .FILL #12640
IGNORE_CST81
JMP R3
CMP_ELSE21
AND R0,R0,#0
CMP_ENDELSE21
ADD R0,R0,#0
BRnp IGNORE_JMP_WHILE17
LD R3,CST83
BR IGNORE_CST83
CST83 .FILL #12899
IGNORE_CST83
JMP R3
IGNORE_JMP_WHILE17
LD R0,CST41
BR IGNORE_CST41
CST41 .FILL #100
IGNORE_CST41
NOT R0,R0
ADD R0,R0,#1
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
STR R0,R1,#0
STARTWHILE22
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
LD R0,CST25
BR IGNORE_CST25
CST25 .FILL #100
IGNORE_CST25
ADD R6,R6,#1
LDR R1,R6,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
ADD R0,R0,#0
BRn IGNORE_JMP_IF26
LD R3,CST85
BR IGNORE_CST85
CST85 .FILL #12686
IGNORE_CST85
JMP R3
IGNORE_JMP_IF26
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST87
BR IGNORE_CST87
CST87 .FILL #12687
IGNORE_CST87
JMP R3
CMP_ELSE26
AND R0,R0,#0
CMP_ENDELSE26
ADD R0,R0,#0
BRnp IGNORE_JMP_WHILE22
LD R3,CST89
BR IGNORE_CST89
CST89 .FILL #12885
IGNORE_CST89
JMP R3
IGNORE_JMP_WHILE22
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
AND R0,R0,#0
ADD R0,R0,#0
ADD R6,R6,#1
LDR R1,R6,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
ADD R0,R0,#0
BRz IGNORE_JMP_IF29
LD R3,CST91
BR IGNORE_CST91
CST91 .FILL #12721
IGNORE_CST91
JMP R3
IGNORE_JMP_IF29
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST93
BR IGNORE_CST93
CST93 .FILL #12722
IGNORE_CST93
JMP R3
CMP_ELSE29
AND R0,R0,#0
CMP_ENDELSE29
ADD R0,R0,#0
BRnp IGNORE_JMP_IF30
LD R3,CST95
BR IGNORE_CST95
CST95 .FILL #12734
IGNORE_CST95
JMP R3
IGNORE_JMP_IF30
AND R0,R0,#0
ADD R0,R0,#0
LD R3,CST97
BR IGNORE_CST97
CST97 .FILL #12736
IGNORE_CST97
JMP R3
EIF_ELSE30
AND R0,R0,#0
ADD R0,R0,#1
EIF_ENDELSE30
ADD R0,R0,#0
BRnp IGNORE_JMP_IF39
LD R3,CST99
BR IGNORE_CST99
CST99 .FILL #12851
IGNORE_CST99
JMP R3
IGNORE_JMP_IF39
AND R1,R1,#0
ADD R1,R1,#1
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
ADD R6,R6,#1
LDR R1,R6,#0
LD R3,CST101
BR IGNORE_CST101
CST101 .FILL #12314
IGNORE_CST101
JSRR R3
STR R0,R6,#0
ADD R6,R6,#-1
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
ADD R6,R6,#1
LDR R1,R6,#0
LD R3,CST103
BR IGNORE_CST103
CST103 .FILL #12299
IGNORE_CST103
JSRR R3
STR R0,R6,#0
ADD R6,R6,#-1
AND R1,R1,#0
ADD R1,R1,#1
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
ADD R6,R6,#1
LDR R1,R6,#0
LD R3,CST105
BR IGNORE_CST105
CST105 .FILL #12347
IGNORE_CST105
JSRR R3
ADD R6,R6,#1
LDR R1,R6,#0
ADD R0,R0,R1
STR R0,R6,#0
ADD R6,R6,#-1
AND R1,R1,#0
ADD R1,R1,#1
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
ADD R6,R6,#1
LDR R1,R6,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
ADD R0,R0,#0
BRz IGNORE_JMP_IF37
LD R3,CST107
BR IGNORE_CST107
CST107 .FILL #12832
IGNORE_CST107
JMP R3
IGNORE_JMP_IF37
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST109
BR IGNORE_CST109
CST109 .FILL #12833
IGNORE_CST109
JMP R3
CMP_ELSE37
AND R0,R0,#0
CMP_ENDELSE37
ADD R0,R0,#0
BRnp IGNORE_JMP_IF38
LD R3,CST111
BR IGNORE_CST111
CST111 .FILL #12845
IGNORE_CST111
JMP R3
IGNORE_JMP_IF38
AND R0,R0,#0
ADD R0,R0,#0
LD R3,CST113
BR IGNORE_CST113
CST113 .FILL #12847
IGNORE_CST113
JMP R3
EIF_ELSE38
AND R0,R0,#0
ADD R0,R0,#1
EIF_ENDELSE38
LD R3,CST115
BR IGNORE_CST115
CST115 .FILL #12853
IGNORE_CST115
JMP R3
EIF_ELSE39
AND R0,R0,#0
ADD R0,R0,#0
EIF_ENDELSE39
ADD R0,R0,#0
BRnp IGNORE_JMP_IF40
LD R3,CST117
BR IGNORE_CST117
CST117 .FILL #12871
IGNORE_CST117
JMP R3
IGNORE_JMP_IF40
AND R0,R0,#0
ADD R0,R0,#1
NOT R0,R0
ADD R0,R0,#1
LDR R7,R5,#1
LDR R6,R5,#2
LDR R5,R5,#3
RET
LD R3,CST119
BR IGNORE_CST119
CST119 .FILL #12871
IGNORE_CST119
JMP R3
IF_ELSE40
IF_ENDELSE40
AND R0,R0,#0
ADD R0,R0,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R5
ADD R1,R0,#0
LDR R0,R1,#0
ADD R0,R0,#1
STR R0,R1,#0
ADD R0,R0,#-1
LD R3,CST121
BR IGNORE_CST121
CST121 .FILL #12657
IGNORE_CST121
JMP R3
ENDWHILE22
AND R0,R0,#0
ADD R0,R0,#1
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R5
ADD R1,R0,#0
LDR R0,R1,#0
ADD R0,R0,#1
STR R0,R1,#0
ADD R0,R0,#-1
LD R3,CST123
BR IGNORE_CST123
CST123 .FILL #12610
IGNORE_CST123
JMP R3
ENDWHILE17
AND R0,R0,#0
ADD R0,R0,#0
LDR R7,R5,#1
LDR R6,R5,#2
LDR R5,R5,#3
RET
FUN_USER_main
STR R5,R6,#0
ADD R6,R6,#-1
ADD R1,R6,#1
STR R1,R6,#0
ADD R6,R6,#-1
STR R7,R6,#0
ADD R6,R6,#-1
ADD R5,R6,#0
ADD R6,R6,#-1
LD R3,CST125
BR IGNORE_CST125
CST125 .FILL #12589
IGNORE_CST125
JSRR R3
AND R1,R1,#0
ADD R1,R1,#0
ADD R6,R6,R1
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
STR R0,R1,#0
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
LDR R1,R1,#0
ADD R0,R1,#0
STR R0,R6,#0
ADD R6,R6,#-1
LD R3,CST127
BR IGNORE_CST127
CST127 .FILL #12499
IGNORE_CST127
JSRR R3
AND R1,R1,#0
ADD R1,R1,#1
ADD R6,R6,R1
AND R0,R0,#0
LDR R7,R5,#1
LDR R6,R5,#2
LDR R5,R5,#3
RET
STRINGS
STATIC_VAR
.END
;  IGNORE_CST49 <- 12291
;  FUN_MULT <- 12299
;  MULT_CHANGE_SIGN <- 12302
;  MULT_INIT <- 12306
;  MULT_LOOP <- 12309
;  MULT_STOP <- 12313
;  FUN_DIV <- 12314
;  DIV_NOT_NEG_B <- 12320
;  DIV_NOT_NEG_A <- 12325
;  DIV_LOOP <- 12327
;  DIV_END_LOOP <- 12338
;  DIV_NEG_RES <- 12343
;  DIV_END <- 12345
;  FUN_MOD <- 12347
;  MOD_NOT_NEG_B <- 12352
;  MOD_NOT_NEG_A <- 12357
;  MOD_LOOP <- 12357
;  MOD_END_LOOP <- 12367
;  MOD_NEG_RES <- 12371
;  MOD_END <- 12373
;  FUN_USER_print_int <- 12374
;  IGNORE_JMP_IF6 <- 12404
;  CMP_ELSE6 <- 12410
;  CMP_ENDELSE6 <- 12411
;  IGNORE_JMP_IF9 <- 12417
;  IGNORE_CST7 <- 12420
;  IF_ELSE9 <- 12442
;  IF_ENDELSE9 <- 12442
;  IGNORE_CST1 <- 12471
;  FUN_USER_print_int_nl <- 12499
;  IGNORE_JMP_IF13 <- 12529
;  CMP_ELSE13 <- 12535
;  CMP_ENDELSE13 <- 12536
;  IGNORE_JMP_IF17 <- 12542
;  IGNORE_CST14 <- 12560
;  IF_ELSE17 <- 12565
;  IF_ENDELSE17 <- 12565
;  FUN_USER_f <- 12589
;  IGNORE_CST43 <- 12602
;  STARTWHILE17 <- 12610
;  IGNORE_CST20 <- 12622
;  IGNORE_JMP_IF21 <- 12633
;  CMP_ELSE21 <- 12639
;  CMP_ENDELSE21 <- 12640
;  IGNORE_JMP_WHILE17 <- 12646
;  IGNORE_CST41 <- 12649
;  STARTWHILE22 <- 12657
;  IGNORE_CST25 <- 12669
;  IGNORE_JMP_IF26 <- 12680
;  CMP_ELSE26 <- 12686
;  CMP_ENDELSE26 <- 12687
;  IGNORE_JMP_WHILE22 <- 12693
;  IGNORE_JMP_IF29 <- 12715
;  CMP_ELSE29 <- 12721
;  CMP_ENDELSE29 <- 12722
;  IGNORE_JMP_IF30 <- 12728
;  EIF_ELSE30 <- 12734
;  EIF_ENDELSE30 <- 12736
;  IGNORE_JMP_IF39 <- 12742
;  IGNORE_JMP_IF37 <- 12826
;  CMP_ELSE37 <- 12832
;  CMP_ENDELSE37 <- 12833
;  IGNORE_JMP_IF38 <- 12839
;  EIF_ELSE38 <- 12845
;  EIF_ENDELSE38 <- 12847
;  EIF_ELSE39 <- 12851
;  EIF_ENDELSE39 <- 12853
;  IGNORE_JMP_IF40 <- 12859
;  IF_ELSE40 <- 12871
;  IF_ENDELSE40 <- 12871
;  ENDWHILE22 <- 12885
;  ENDWHILE17 <- 12899
;  FUN_USER_main <- 12905
;  STRINGS <- 12948
;  STATIC_VAR <- 12948
;  .END <- 12948
