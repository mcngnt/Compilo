.ORIG x3000
;
; --- START OF THE PROGRAMM ---
;
LD R6 STACK
LD R5 STACK
LEA R4 STATIC
ADD R4 R4 #1
LD R3 CST7
JMP R3 ; Jump to label FUN_USER_main
BR IGNORE_CST7
CST7 .FILL #12311
IGNORE_CST7
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
FUN_USER_main
ADD R6 R6 #-1
LDR R5 R6 #0
ADD R6 R6 #-1
LDR R7 R6 #0
ADD R6 R6 #-1
AND R5 R5 #0
ADD R5 R5 R6
ADD R6 R6 #-1 ; Add variable x to the stack 
ADD R6 R6 #-1 ; Add variable y to the stack 
LD R0 CST6 ; R0 <- cst 3
BR IGNORE_CST6
CST6 .FILL #3
IGNORE_CST6
STR R0 R5 #0; variable x <- R0
LD R0 CST5 ; R0 <- cst 2
BR IGNORE_CST5
CST5 .FILL #2
IGNORE_CST5
STR R0 R5 #-1; variable y <- R0
STARTWHILE0
LD R0 CST4 ; R0 <- cst 0
BR IGNORE_CST4
CST4 .FILL #0
IGNORE_CST4
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LDR R0 R5 #0; R0 <- variable x
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ;  Retrieve upmost result on the stack in R1
NOT R0 R0
ADD R0 R0 #1 ;  R0 <- -R0
ADD R0 R0 R1 ; Compute R1 - R0 for comparison
BRn IGNORE_GOTO9
LD R3 CST8
JMP R3 ; Jump to label CMP_ELSE3
BR IGNORE_CST8
CST8 .FILL #12350
IGNORE_CST8
IGNORE_GOTO9
AND R0 R0 #0
ADD R0 R0 #1 ; If comparison true, set R0 to 1
LD R3 CST10
JMP R3 ; Jump to label CMP_ENDELSE3
BR IGNORE_CST10
CST10 .FILL #12351
IGNORE_CST10
CMP_ELSE3
AND R0 R0 #0 ; If comparison false, set R0 to 0
CMP_ENDELSE3
; R0 <-  e1 > e2
BRz ENDWHILE0
LDR R0 R5 #0; R0 <- variable x
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LD R0 CST2 ; R0 <- cst 1
BR IGNORE_CST2
CST2 .FILL #1
IGNORE_CST2
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ; Retrieve upmost result on the stack in R1
NOT R0 R0
ADD R0 R0 #1 ;  R0 <- -R0
ADD R0 R0 R1 ; R0 <- R1 - R0
STR R0 R5 #0; variable x <- R0
LDR R0 R5 #-1; R0 <- variable y
STR R0 R6 #0 ; Store R0 on the stack
ADD R6 R6 #-1 ; Increase the stack
LDR R0 R5 #-1; R0 <- variable y
ADD R6 R6 #1 ; Decrease the stack
LDR R1 R6 #0 ; Retrieve upmost result on the stack in R1
LD R3 CST11
JSRR R3 ; Jump to label FUN_MULT
BR IGNORE_CST11
CST11 .FILL #12296
IGNORE_CST11
STR R0 R5 #-1; variable y <- R0
BR STARTWHILE0
ENDWHILE0
LDR R0 R5 #-1; R0 <- variable y
STR R0 R4 #0; variable glob <- R0
LDR R0 R5 #-1; R0 <- variable y
LDR R7 R5 #-1
LDR R5 R5 #-2
;
; --- STATIC VARIABLES ---
;
STATIC .BLKW #1
V_glob .BLKW #1
STACK .FILL xfdff
.END
; FUN_MULT x3008
; MULT_CHANGE_SIGN x300b
; MULT_INIT x300f
; MULT_LOOP x3012
; MULT_STOP x3016
; FUN_USER_main x3017
; IGNORE_CST6 x3023
; IGNORE_CST5 x3027
; STARTWHILE0 x3028
; IGNORE_CST4 x302b
; CMP_ELSE3 x303e
; CMP_ENDELSE3 x303f
; IGNORE_CST2 x3046
; ENDWHILE0 x3058
; .END x3060
; --DEBUG---

