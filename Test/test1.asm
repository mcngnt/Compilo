.ORIG x3000
LD R6 CST34 ; Init R6 value to the start of the stack
BR IGNORE_CST34
CST34 .FILL #65023
IGNORE_CST34
ADD R5 R6 #0
LD R4 CST35 ; R4 <- address of label STATIC_VAR
BR IGNORE_CST35
CST35 .FILL #12735
IGNORE_CST35
ADD R4 R4 #1
LD R3 CST36 ; R3 <- address of label FUN_USER_main
BR IGNORE_CST36
CST36 .FILL #12377
IGNORE_CST36
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
ADD R6 R6 #-1 ; Add variable n to the stack 
ADD R6 R6 #-1 ; Add variable start to the stack 
ADD R6 R6 #-1 ; Add variable p to the stack 
LD R0 CST32 ; R0 <- cst 12
BR IGNORE_CST32
CST32 .FILL #12
IGNORE_CST32
LD R1 CST33
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
STR R0 R1 #0 ; Change variable content to R0
BR IGNORE_CST33
CST33 .FILL #0
IGNORE_CST33
LD R0 CST37 ; R0 <- address of label STRING1
BR IGNORE_CST37
CST37 .FILL #12654
IGNORE_CST37
LD R1 CST31
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
STR R0 R1 #0 ; Change variable content to R0
BR IGNORE_CST31
CST31 .FILL #1
IGNORE_CST31
LD R0 CST28
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST28
CST28 .FILL #1
IGNORE_CST28
LD R1 CST29
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST29
CST29 .FILL #1
IGNORE_CST29
LD R1 CST30
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
STR R0 R1 #0 ; Change variable content to R0
BR IGNORE_CST30
CST30 .FILL #2
IGNORE_CST30
LD R0 CST26 ; R0 <- cst 0
BR IGNORE_CST26
CST26 .FILL #0
IGNORE_CST26
LD R1 CST27
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
BR IGNORE_CST27
CST27 .FILL #2
IGNORE_CST27

STR R0 R1 #0 ; R0 <- M[R1]
LD R0 CST24
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST24
CST24 .FILL #2
IGNORE_CST24
LD R1 CST25
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST25
CST25 .FILL #2
IGNORE_CST25
LDR R1 R4 #-1
LDR R0 R1 #0
ADD R0 R0 #1
STR R0 R1 #0 ; ++(lvalue)
LD R0 CST22 ; R0 <- cst 1
BR IGNORE_CST22
CST22 .FILL #1
IGNORE_CST22
LD R1 CST23
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
BR IGNORE_CST23
CST23 .FILL #2
IGNORE_CST23

STR R0 R1 #0 ; R0 <- M[R1]
STARTWHILE4
LD R0 CST20
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST20
CST20 .FILL #2
IGNORE_CST20
LD R1 CST21
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST21
CST21 .FILL #2
IGNORE_CST21
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LD R0 CST18
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST18
CST18 .FILL #1
IGNORE_CST18
LD R1 CST19
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST19
CST19 .FILL #1
IGNORE_CST19
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LD R0 CST16
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST16
CST16 .FILL #0
IGNORE_CST16
LD R1 CST17
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST17
CST17 .FILL #0
IGNORE_CST17
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ; Retrieve upmost result on the stack in R1
ADD R0 R0 R1 ; R0 <- R0 + R1
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ;  Retrieve upmost result on the stack in R1
NOT R0 R0
ADD R0 R0 #1 ;  R0 <- -R0
ADD R0 R0 R1 ; Compute R1 - R0 for comparison
BRn IGNORE_JMP15
LD R3 CST38 ; R3 <- address of label CMP_ELSE15
BR IGNORE_CST38
CST38 .FILL #12537
IGNORE_CST38
JMP R3
IGNORE_JMP15
AND R0 R0 #0
ADD R0 R0 #1 ; If comparison true, set R0 to 1
LD R3 CST39 ; R3 <- address of label CMP_ENDELSE15
BR IGNORE_CST39
CST39 .FILL #12538
IGNORE_CST39
JMP R3
CMP_ELSE15
AND R0 R0 #0 ; If comparison false, set R0 to 0
CMP_ENDELSE15
; R0 <-  e1 > e2
BRz ENDWHILE4
LD R0 CST13
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST13
CST13 .FILL #2
IGNORE_CST13
LD R1 CST14
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST14
CST14 .FILL #2
IGNORE_CST14
LDR R1 R4 #-1
LDR R0 R1 #0
ADD R0 R0 #1
STR R0 R1 #0 ; ++(lvalue)
LD R0 CST10
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST10
CST10 .FILL #2
IGNORE_CST10
LD R1 CST11
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST11
CST11 .FILL #2
IGNORE_CST11
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LD R0 CST9 ; R0 <- cst 1
BR IGNORE_CST9
CST9 .FILL #1
IGNORE_CST9
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ; Retrieve upmost result on the stack in R1
NOT R0 R0
ADD R0 R0 #1 ;  R0 <- -R0
ADD R0 R0 R1 ; R0 <- R1 - R0
STR R0 R4 #-1
LDR R0 R0 #0 ; Deref the last lvalue
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LD R0 CST7
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST7
CST7 .FILL #2
IGNORE_CST7
LD R1 CST8
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST8
CST8 .FILL #2
IGNORE_CST8
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LD R0 CST6 ; R0 <- cst 2
BR IGNORE_CST6
CST6 .FILL #2
IGNORE_CST6
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ; Retrieve upmost result on the stack in R1
NOT R0 R0
ADD R0 R0 #1 ;  R0 <- -R0
ADD R0 R0 R1 ; R0 <- R1 - R0
STR R0 R4 #-1
LDR R0 R0 #0 ; Deref the last lvalue
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ; Retrieve upmost result on the stack in R1
ADD R0 R0 R1 ; R0 <- R0 + R1
LD R1 CST12
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
BR IGNORE_CST12
CST12 .FILL #2
IGNORE_CST12

STR R0 R1 #0 ; R0 <- M[R1]
BR STARTWHILE4
ENDWHILE4
LD R0 CST2
NOT R0 R0
ADD R0 R0 #1
ADD R0 R0 R5
STR R0 R4 #-1 ; Change lvalue
BR IGNORE_CST2
CST2 .FILL #2
IGNORE_CST2
LD R1 CST3
NOT R1 R1
ADD R1 R1 #1
ADD R1 R5 R1
LDR R1 R1 #0 ; Put variable content in R0
ADD R0 R1 #0
BR IGNORE_CST3
CST3 .FILL #2
IGNORE_CST3
STR R0 R4 #-1
LDR R0 R0 #0 ; Deref the last lvalue
LD R1 CST4
ADD R1 R4 R1
STR R0 R1 #0 ; Change variable content to R0
BR IGNORE_CST4
CST4 .FILL #0
IGNORE_CST4
LD R0 CST1 ; R0 <- cst 0
BR IGNORE_CST1
CST1 .FILL #0
IGNORE_CST1
LDR R7 R5 #1 ; Restore R7
LDR R5 R5 #2 ; Restore R5
RET
STRINGS
STRING1 .STRINGZ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ; 80
STATIC_VAR
LVALUE_ADDR .BLKW #1
V_res .BLKW #1

.END
; mem 12735 12736
