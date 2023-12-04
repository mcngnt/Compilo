.ORIG x3000
LD R6 CST8 ; Init R6 value to the start of the stack
BR IGNORE_CST8
CST8 .FILL #65023
IGNORE_CST8
ADD R5 R6 #0
LD R4 CST9 ; R4 <- address of label STATIC_VAR
BR IGNORE_CST9
CST9 .FILL #12439
IGNORE_CST9
ADD R4 R4 #1
LD R3 CST10 ; R3 <- address of label FUN_USER_main
BR IGNORE_CST10
CST10 .FILL #12377
IGNORE_CST10
JMP R3
FUN_MULT
ADD R0 R0 #0
BRn MULT_CHANGE_SIGN
BR MULT_INIT
MULT_CHANGE_SIGN
NOT R0 R0
ADD R0 R0 #1
NOT R1 R1
ADD R1 R1 #1
MULT_INIT
AND R2 R2 #0
ADD R2 R2 R0
AND R0 R0 #0
MULT_LOOP
ADD R0 R0 R1
ADD R2 R2 #-1
BRz MULT_STOP
BR MULT_LOOP
MULT_STOP
RET
FUN_DIV
AND R2 R2 #0 ; Q
LD R2 DIV_ISNEG ; Set is_neg to 0
ADD R1 R1 #0
BRn DIV_A_NEG
BR DIV_A_POS
DIV_A_NEG
NOT R1 R1 ; Change A sign when negative
ADD R1 R1 #1
ADD R0 R0 #0
BRn DIV_AN_BN
BR DIV_AN_BP
DIV_A_POS
ADD R0 R0 #0
BRn DIV_AP_BN
BR DIV_AP_BP
DIV_AN_BN
NOT R0 R0
ADD R0 R0 #1
BR DIV_POS
DIV_AN_BP
BR DIV_NEG
DIV_AP_BN
NOT R0 R0
ADD R0 R0 #1
BR DIV_NEG
DIV_AP_BP
BR DIV_POS
DIV_NEG
ADD R3 R2 #1
ST R3 DIV_ISNEG ; IS_NEG <- 1
BR DIV_ENDSIGN
DIV_POS
BR DIV_ENDSIGN
DIV_ENDSIGN
; Compute the sign of the result in DIV_ISNEG
DIV_LOOP
NOT R3 R1
ADD R3 R3 #1
ADD R3 R3 R0 ; R3 <- R0 - R1 = B - R
BRp DIV_END_LOOP
AND R3 R3 #0
ADD R3 R3 R0
NOT R3 R3
ADD R3 R3 #1 ; R3 <- -B
ADD R1 R1 R3 ; R <- R - B
ADD R2 R2 #1
BR DIV_LOOP
DIV_END_LOOP
ADD R0 R2 #0
LD R3 DIV_ISNEG
BRz DIV_END
NOT R0 R0
ADD R0 R0 #1
DIV_END
RET
DIV_ISNEG .BLKW #1
FUN_MOD
AND R2 R2 #0 ; Q
ADD R1 R1 #0
BRn MOD_A_NEG
BR MOD_ENDSIGN
MOD_A_NEG
ADD R1 R1 R0
BRn MOD_A_NEG
MOD_ENDSIGN
; Compute the sign of the result in MOD_ISNEG
MOD_LOOP
NOT R3 R1
ADD R3 R3 #1
ADD R3 R3 R0 ; R3 <- R0 - R1 = B - R
BRp MOD_END_LOOP
AND R3 R3 #0
ADD R3 R3 R0
NOT R3 R3
ADD R3 R3 #1 ; R3 <- -B
ADD R1 R1 R3 ; R <- R - B
ADD R2 R2 #1
BR MOD_LOOP
MOD_END_LOOP
ADD R0 R1 #0
MOD_END
RET
FUN_USER_main
ADD R6 R6 #-1
STR R5 R6 #0 ; Store R5 on the stack
ADD R6 R6 #-1
STR R7 R6 #0 ; Store R7 on the stack
ADD R6 R6 #-1
ADD R5 R6 #0 ; R5 <- R6
STARTWHILE1
GETC
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LD R0 CST4 ; R0 <- cst 101
BR IGNORE_CST4
CST4 .FILL #101
IGNORE_CST4
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ;  Retrieve upmost result on the stack in R1
NOT R0 R0
ADD R0 R0 #1 ;  R0 <- -R0
ADD R0 R0 R1 ; Compute R1 - R0 for comparison
ADD R0 R0 #0
BRz IGNORE_JMP3
LD R3 CST11 ; R3 <- address of label CMP_ELSE3
BR IGNORE_CST11
CST11 .FILL #12406
IGNORE_CST11
JMP R3
IGNORE_JMP3
AND R0 R0 #0
ADD R0 R0 #1 ; If comparison true, set R0 to 1
LD R3 CST12 ; R3 <- address of label CMP_ENDELSE3
BR IGNORE_CST12
CST12 .FILL #12407
IGNORE_CST12
JMP R3
CMP_ELSE3
AND R0 R0 #0 ; If comparison false, set R0 to 0
CMP_ENDELSE3
; R0 <-  e1 == e2
ADD R0 R0 #0
BRnp IGNORE_JMP7
LD R3 CST13 ; R3 <- address of label EIF_ELSE7
BR IGNORE_CST13
CST13 .FILL #12420
IGNORE_CST13
JMP R3
IGNORE_JMP7
LD R0 CST5 ; R0 <- cst 0
BR IGNORE_CST5
CST5 .FILL #0
IGNORE_CST5
LD R3 CST14 ; R3 <- address of label EIF_ENDELSE7
BR IGNORE_CST14
CST14 .FILL #12423
IGNORE_CST14
JMP R3
EIF_ELSE7
LD R0 CST6 ; R0 <- cst 1
BR IGNORE_CST6
CST6 .FILL #1
IGNORE_CST6
EIF_ENDELSE7
ADD R0 R0 #0
BRnp IGNORE_JMP1
LD R3 CST15 ; R3 <- address of label ENDWHILE1
BR IGNORE_CST15
CST15 .FILL #12433
IGNORE_CST15
JMP R3
IGNORE_JMP1
LD R3 CST16 ; R3 <- address of label STARTWHILE1
BR IGNORE_CST16
CST16 .FILL #12383
IGNORE_CST16
JMP R3
ENDWHILE1
LD R0 CST1 ; R0 <- cst 0
BR IGNORE_CST1
CST1 .FILL #0
IGNORE_CST1
LDR R7 R5 #1 ; Restore R7
LDR R5 R5 #2 ; Restore R5
RET
STRINGS
STATIC_VAR
LVALUE_ADDR .BLKW #1
V_res .BLKW #1

.END
; mem 12439 12440
