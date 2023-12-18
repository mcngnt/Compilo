.ORIG #12288
LD R6,CST29
BR IGNORE_CST29
CST29 .FILL #65023
IGNORE_CST29
ADD R5,R6,#0
LD R4,CST31
BR IGNORE_CST31
CST31 .FILL #12729
IGNORE_CST31
LD R3,CST33
BR IGNORE_CST33
CST33 .FILL #12695
IGNORE_CST33
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
LD R3,CST35
BR IGNORE_CST35
CST35 .FILL #12410
IGNORE_CST35
JMP R3
IGNORE_JMP_IF6
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST37
BR IGNORE_CST37
CST37 .FILL #12411
IGNORE_CST37
JMP R3
CMP_ELSE6
AND R0,R0,#0
CMP_ENDELSE6
ADD R0,R0,#0
BRnp IGNORE_JMP_IF9
LD R3,CST39
BR IGNORE_CST39
CST39 .FILL #12442
IGNORE_CST39
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
LD R3,CST41
BR IGNORE_CST41
CST41 .FILL #12442
IGNORE_CST41
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
LD R3,CST43
BR IGNORE_CST43
CST43 .FILL #12314
IGNORE_CST43
JSRR R3
STR R0,R6,#0
ADD R6,R6,#-1
LD R3,CST45
BR IGNORE_CST45
CST45 .FILL #12374
IGNORE_CST45
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
LD R3,CST47
BR IGNORE_CST47
CST47 .FILL #12347
IGNORE_CST47
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
LD R3,CST49
BR IGNORE_CST49
CST49 .FILL #12535
IGNORE_CST49
JMP R3
IGNORE_JMP_IF13
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST51
BR IGNORE_CST51
CST51 .FILL #12536
IGNORE_CST51
JMP R3
CMP_ELSE13
AND R0,R0,#0
CMP_ENDELSE13
ADD R0,R0,#0
BRnp IGNORE_JMP_IF17
LD R3,CST53
BR IGNORE_CST53
CST53 .FILL #12565
IGNORE_CST53
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
LD R3,CST55
BR IGNORE_CST55
CST55 .FILL #12565
IGNORE_CST55
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
LD R3,CST57
BR IGNORE_CST57
CST57 .FILL #12374
IGNORE_CST57
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
FUN_USER_sq
STR R5,R6,#0
ADD R6,R6,#-1
ADD R1,R6,#1
STR R1,R6,#0
ADD R6,R6,#-1
STR R7,R6,#0
ADD R6,R6,#-1
ADD R5,R6,#0
ADD R6,R6,#-1
AND R0,R0,#0
ADD R0,R0,#0
AND R1,R1,#0
ADD R1,R1,#0
NOT R1,R1
ADD R1,R1,#1
ADD R1,R5,R1
STR R0,R1,#0
STARTWHILE18
AND R1,R1,#0
ADD R1,R1,#0
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
LD R3,CST59
BR IGNORE_CST59
CST59 .FILL #12299
IGNORE_CST59
JSRR R3
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
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
ADD R0,R0,#0
BRnz IGNORE_JMP_IF23
LD R3,CST61
BR IGNORE_CST61
CST61 .FILL #12654
IGNORE_CST61
JMP R3
IGNORE_JMP_IF23
AND R0,R0,#0
ADD R0,R0,#1
LD R3,CST63
BR IGNORE_CST63
CST63 .FILL #12655
IGNORE_CST63
JMP R3
CMP_ELSE23
AND R0,R0,#0
CMP_ENDELSE23
ADD R0,R0,#0
BRnp IGNORE_JMP_WHILE18
LD R3,CST65
BR IGNORE_CST65
CST65 .FILL #12675
IGNORE_CST65
JMP R3
IGNORE_JMP_WHILE18
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
LD R3,CST67
BR IGNORE_CST67
CST67 .FILL #12606
IGNORE_CST67
JMP R3
ENDWHILE18
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
ADD R0,R0,#1
ADD R6,R6,#1
LDR R1,R6,#0
NOT R0,R0
ADD R0,R0,#1
ADD R0,R0,R1
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
LD R0,CST26
BR IGNORE_CST26
CST26 .FILL #99
IGNORE_CST26
STR R0,R6,#0
ADD R6,R6,#-1
LD R3,CST69
BR IGNORE_CST69
CST69 .FILL #12589
IGNORE_CST69
JSRR R3
AND R1,R1,#0
ADD R1,R1,#1
ADD R6,R6,R1
STR R0,R6,#0
ADD R6,R6,#-1
LD R3,CST71
BR IGNORE_CST71
CST71 .FILL #12499
IGNORE_CST71
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
;  IGNORE_CST29 <- 12291
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
;  FUN_USER_sq <- 12589
;  STARTWHILE18 <- 12606
;  IGNORE_JMP_IF23 <- 12648
;  CMP_ELSE23 <- 12654
;  CMP_ENDELSE23 <- 12655
;  IGNORE_JMP_WHILE18 <- 12661
;  ENDWHILE18 <- 12675
;  FUN_USER_main <- 12695
;  IGNORE_CST26 <- 12706
;  STRINGS <- 12729
;  STATIC_VAR <- 12729
;  .END <- 12729
