;--------------------------------------------
; EMU8086 Ping Pong Game - Viva Version
; Annotated for: Color, Difficulty, Countdown
;--------------------------------------------

.MODEL SMALL
.STACK 100h

.DATA
; Ball 1 position and direction
ballX       db 40
ballY       db 12
ballDX      db 2
ballDY      db 1

; BALL 2
ball2X      db 40
ball2Y      db 15
ball2DX     db -2
ball2DY     db -1
ball2Active db 0

; Paddles (height = 4)
paddleA     db 10,11,12,13
paddleB     db 10,11,12,13

; Scores
scoreA      db 0
scoreB      db 0
WIN_SCORE   db 5

; -------------------------------------------------------------
; [FEATURE: DIFFICULTY LEVEL START]
; -------------------------------------------------------------
; Speed settings: These variables control the difficulty.
ballSpeedH  db 2    ; This byte holds horizontal speed (1=Slow, 2=Med, 3=Fast)
paddleSpeed db 2    ; This byte holds paddle speed
; -------------------------------------------------------------
; [FEATURE: DIFFICULTY LEVEL END]
; -------------------------------------------------------------

; -------------------------------------------------------------
; [FEATURE: COLOR GRAPHICS START]
; -------------------------------------------------------------
; Color Definitions: Used to put color values into BL register later.
COLOR_A     db 0Ch ; 0Ch = Black Background (0) + Red Text (C)
COLOR_B     db 0Bh ; 0Bh = Black Background (0) + Cyan Text (B)
COLOR_BALL  db 0Eh ; 0Eh = Black Background (0) + Yellow Text (E)
COLOR_BALL2 db 0Dh ; 0Dh = Black Background (0) + Magenta Text (D)
COLOR_BG    db 00h ; 00h = Black Background (0) + Black Text (0)
; -------------------------------------------------------------
; [FEATURE: COLOR GRAPHICS END]
; -------------------------------------------------------------

; GRAPHICS CHARACTERS
CHAR_PADDLE     db 219
CHAR_BALL       db 0Fh

; Messages - FIXED NO SPECIAL CHARACTERS
msgMenu     db 13,10
            db 'PING PONG GAME',13,10,13,10
            db 'Choose Difficulty:',13,10
            db '1 Easy',13,10
            db '2 Medium',13,10
            db '3 Hard',13,10,13,10
            db 'Press 1 2 or 3: $'
msgAWins    db 13,10,13,10
            db 'PLAYER A WINS!',13,10,13,10
            db 'Press R to Restart or any other key to Exit: $'
msgBWins    db 13,10,13,10
            db 'PLAYER B WINS!',13,10,13,10
            db 'Press R to Restart or any other key to Exit: $'
msgBall2    db 'MULTI BALL!$'

; -------------------------------------------------------------
; [FEATURE: COUNTDOWN START]
; -------------------------------------------------------------
; Countdown Messages: These strings are printed one by one.
msg3        db '3$' ; '$' tells INT 21h where the string ends
msg2        db '2$'
msg1        db '1$'
msgGo       db 'GO!$' 
; -------------------------------------------------------------
; [FEATURE: COUNTDOWN END]
; -------------------------------------------------------------

playerAText db 'PLAYER A',0
playerBText db 'PLAYER B',0

.CODE
MAIN PROC
    mov ax, @DATA
    mov ds, ax

StartGame:
    call ClearScreenProperly
    call ShowMenu
    
    ; -------------------------------------------------------------
    ; [FEATURE: DIFFICULTY LEVEL START]
    ; -------------------------------------------------------------
    call SelectLevel ; Logic is inside this procedure (Scroll down to find it)
    ; -------------------------------------------------------------
    ; [FEATURE: DIFFICULTY LEVEL END]
    ; -------------------------------------------------------------
    
    call ClearScreenProperly

    mov scoreA, 0
    mov scoreB, 0
    mov ball2Active, 0
    
    mov paddleA, 10
    mov paddleA+1, 11
    mov paddleA+2, 12
    mov paddleA+3, 13
    mov paddleB, 10
    mov paddleB+1, 11
    mov paddleB+2, 12
    mov paddleB+3, 13

    ; -------------------------------------------------------------
    ; [FEATURE: COUNTDOWN START]
    ; -------------------------------------------------------------
    call CountdownStart ; Logic is inside this procedure
    ; -------------------------------------------------------------
    ; [FEATURE: COUNTDOWN END]
    ; -------------------------------------------------------------

    call DrawBoard

GameLoop:
    mov ah, 01h
    int 16h
    jz NoKeyPressed
    
    mov ah, 00h
    int 16h
    call ProcessKey

NoKeyPressed:
    call UpdateBall
    
    cmp ball2Active, 1
    jne SkipBall2
    call UpdateBall2
    
SkipBall2:
    mov al, scoreA
    cmp al, WIN_SCORE
    jae PlayerAWon
    mov al, scoreB
    cmp al, WIN_SCORE
    jae PlayerBWon
    
    jmp GameLoop

PlayerAWon:
    call ClearScreenProperly
    call VictorySound
    
    mov ah, 2
    mov dh, 10
    mov dl, 0
    mov bh, 0
    int 10h
    
    mov ah, 9
    lea dx, msgAWins
    int 21h
    jmp CheckRestart

PlayerBWon:
    call ClearScreenProperly
    call VictorySound
    
    mov ah, 2
    mov dh, 10
    mov dl, 0
    mov bh, 0
    int 10h
    
    mov ah, 9
    lea dx, msgBWins
    int 21h

CheckRestart:
    mov ah, 0
    int 16h
    
    cmp al, 'R'
    je StartGame
    cmp al, 'r'
    je StartGame

ExitGame:
    ; RESTORE CURSOR BEFORE EXITING
    mov ah, 1
    mov cx, 0607h
    int 10h
    
    mov ah, 4Ch
    int 21h
MAIN ENDP

ClearScreenProperly PROC
    push ax
    push bx
    push cx
    push dx
    
    mov ah, 0
    mov al, 3
    int 10h
    
    mov ah, 1
    mov cx, 2607h
    int 10h
    
    mov ah, 06h      
    mov al, 0        
    mov bh, 07h      
    mov cx, 0        
    mov dx, 184Fh    
    int 10h
    
    mov ah, 2
    mov bh, 0
    mov dx, 0
    int 10h
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
ClearScreenProperly ENDP

; -------------------------------------------------------------
; [FEATURE: COUNTDOWN START]
; -------------------------------------------------------------
CountdownStart PROC
    call ClearScreenProperly ; Wipe screen first
    
    ; --- STEP 1: PRINT "3" ---
    mov dh, 12      ; DH register holds Row 12 (Y-coord)
    mov dl, 39      ; DL register holds Column 39 (X-coord)
    mov bh, 0       ; BH register holds Page 0
    mov ah, 2       ; AH register holds Function 2 (Set Cursor)
    int 10h         ; Execute Move
    mov ah, 9       ; AH register holds Function 9 (Print String)
    lea dx, msg3    ; DX register holds memory address of "3" message
    int 21h         ; Execute Print
    call Beep       ; Play sound
    call LongDelay  ; Wait 1 second (Uses Timer)
    
    ; --- STEP 2: PRINT "2" ---
    mov dh, 12
    mov dl, 39      ; Same position
    mov ah, 2
    int 10h
    mov ah, 9
    lea dx, msg2    ; DX register holds memory address of "2" message
    int 21h
    call Beep
    call LongDelay
    
    ; --- STEP 3: PRINT "1" ---
    mov dh, 12
    mov dl, 39
    mov ah, 2
    int 10h
    mov ah, 9
    lea dx, msg1    ; DX register holds memory address of "1" message
    int 21h
    call Beep
    call LongDelay
    
    ; --- STEP 4: PRINT "GO!" ---
    mov dh, 12
    mov dl, 38      ; Shift Left slightly (Column 38)
    mov ah, 2
    int 10h
    mov ah, 9
    lea dx, msgGo   ; DX register holds memory address of "GO!" message
    int 21h
    call DoubleBeep
    call ShortDelay
    
    call ClearScreenProperly ; Clear text before game starts
    
    ret
CountdownStart ENDP
; -------------------------------------------------------------
; [FEATURE: COUNTDOWN END]
; -------------------------------------------------------------

Beep PROC
    push ax
    push dx
    mov ah, 02h
    mov dl, 07h
    int 21h
    pop dx
    pop ax
    ret
Beep ENDP

DoubleBeep PROC
    call Beep
    call ShortDelay
    call Beep
    ret
DoubleBeep ENDP

VictorySound PROC
    call Beep
    call ShortDelay
    call Beep
    call ShortDelay
    call Beep
    ret
VictorySound ENDP

; -------------------------------------------------------------
; [FEATURE: COUNTDOWN TIMER START]
; -------------------------------------------------------------
LongDelay PROC
    push cx         ; Save CX register
    push dx         ; Save DX register
    
    ; We are setting the wait time to 1,000,000 microseconds (1 second)
    ; Hex value: 0F4240h
    mov cx, 0Fh     ; CX register holds High 16 bits of time
    mov dx, 4240h   ; DX register holds Low 16 bits of time
    mov ah, 86h     ; AH register holds Function 86h (Wait)
    int 15h         ; Call BIOS System Timer
    
    pop dx
    pop cx
    ret
LongDelay ENDP
; -------------------------------------------------------------
; [FEATURE: COUNTDOWN TIMER END]
; -------------------------------------------------------------

ShortDelay PROC
    push cx
    push dx
    mov cx, 07h
    mov dx, 0A120h
    mov ah, 86h
    int 15h
    pop dx
    pop cx
    ret
ShortDelay ENDP

ShowMenu PROC
    mov ah, 2
    mov dh, 0
    mov dl, 0
    mov bh, 0
    int 10h
    
    mov ah, 9
    lea dx, msgMenu
    int 21h
    ret
ShowMenu ENDP

; -------------------------------------------------------------
; [FEATURE: DIFFICULTY LEVEL START]
; -------------------------------------------------------------
SelectLevel PROC
WaitForLevel:
    ; 1. GET USER INPUT
    mov ah, 0       ; AH register holds Function 0 (Get Key)
    int 16h         ; Call BIOS. AL register gets the ASCII code.
    
    ; 2. CHECK INPUT
    cmp al, '1'     ; Compare AL register with '1'
    je EasyLevel    ; If equal, Jump to EasyLevel
    cmp al, '2'     ; Compare AL register with '2'
    je MediumLevel  ; If equal, Jump to MediumLevel
    cmp al, '3'     ; Compare AL register with '3'
    je HardLevel    ; If equal, Jump to HardLevel
    jmp WaitForLevel ; If not 1/2/3, loop back

; 3. APPLY SETTINGS
EasyLevel:
    mov ballSpeedH, 1 ; Write 1 to ballSpeedH variable (Slow)
    mov paddleSpeed, 1
    ret

MediumLevel:
    mov ballSpeedH, 2 ; Write 2 to ballSpeedH variable (Medium)
    mov paddleSpeed, 2
    ret

HardLevel:
    mov ballSpeedH, 3 ; Write 3 to ballSpeedH variable (Fast)
    mov paddleSpeed, 3
    ret
SelectLevel ENDP
; -------------------------------------------------------------
; [FEATURE: DIFFICULTY LEVEL END]
; -------------------------------------------------------------

; -------------------------------------------------------------
; [FEATURE: COLOR GRAPHICS START]
; -------------------------------------------------------------
DrawBoard PROC
    mov si, 0
    mov cx, 4
DrawPaddleALoop:
    mov dh, [paddleA+si]
    mov dl, 2
    mov al, CHAR_PADDLE
    mov bl, COLOR_A      ; BL register gets 0Ch (Red Color) <--- COLOR HERE
    call PutChar         ; Call helper function to draw
    inc si
    loop DrawPaddleALoop

    mov si, 0
    mov cx, 4
DrawPaddleBLoop:
    mov dh, [paddleB+si]
    mov dl, 77
    mov al, CHAR_PADDLE
    mov bl, COLOR_B      ; BL register gets 0Bh (Cyan Color) <--- COLOR HERE
    call PutChar
    inc si
    loop DrawPaddleBLoop

    mov dh, ballY
    mov dl, ballX
    mov al, CHAR_BALL
    mov bl, COLOR_BALL   ; BL register gets 0Eh (Yellow Color) <--- COLOR HERE
    call PutChar
    
    cmp ball2Active, 1
    jne SkipDrawBall2
    mov dh, ball2Y
    mov dl, ball2X
    mov al, CHAR_BALL
    mov bl, COLOR_BALL2  ; BL register gets 0Dh (Magenta Color) <--- COLOR HERE
    call PutChar

SkipDrawBall2:
    call ShowScore
    ret
DrawBoard ENDP
; -------------------------------------------------------------
; [FEATURE: COLOR GRAPHICS END]
; -------------------------------------------------------------

ProcessKey PROC
    cmp al, 'w'
    je MoveAUp
    cmp al, 'W'
    je MoveAUp
    cmp al, 's'
    je MoveADown
    cmp al, 'S'
    je MoveADown
    cmp al, 'i'
    je MoveBUp
    cmp al, 'I'
    je MoveBUp
    cmp al, 'k'
    je MoveBDown
    cmp al, 'K'
    je MoveBDown
    ret

MoveAUp:
    mov al, paddleSpeed
    mov bl, byte ptr [paddleA]
    sub bl, al
    cmp bl, 1
    jl NoMove
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 3
EraseAUpLoop:
    mov dh, [paddleA+si]
    mov dl, 2
    push ax
    mov al, ' '
    mov bl, COLOR_BG
    call PutChar
    pop ax
    dec si
    loop EraseAUpLoop
    
    mov al, paddleSpeed
    sub byte ptr [paddleA], al
    sub byte ptr [paddleA+1], al
    sub byte ptr [paddleA+2], al
    sub byte ptr [paddleA+3], al
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 0
DrawAUpLoop:
    mov dh, [paddleA+si]
    mov dl, 2
    push ax
    mov al, CHAR_PADDLE
    mov bl, COLOR_A
    call PutChar
    pop ax
    inc si
    loop DrawAUpLoop
NoMove:
    ret

MoveADown:
    mov al, paddleSpeed
    mov bl, byte ptr [paddleA+3]
    add bl, al
    cmp bl, 23
    jg NoMove
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 0
EraseADownLoop:
    mov dh, [paddleA+si]
    mov dl, 2
    push ax
    mov al, ' '
    mov bl, COLOR_BG
    call PutChar
    pop ax
    inc si
    loop EraseADownLoop
    
    mov al, paddleSpeed
    add byte ptr [paddleA], al
    add byte ptr [paddleA+1], al
    add byte ptr [paddleA+2], al
    add byte ptr [paddleA+3], al
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 3
DrawADownLoop:
    mov dh, [paddleA+si]
    mov dl, 2
    push ax
    mov al, CHAR_PADDLE
    mov bl, COLOR_A
    call PutChar
    pop ax
    dec si
    loop DrawADownLoop
    ret

MoveBUp:
    mov al, paddleSpeed
    mov bl, byte ptr [paddleB]
    sub bl, al
    cmp bl, 1
    jl NoMove
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 3
EraseBUpLoop:
    mov dh, [paddleB+si]
    mov dl, 77
    push ax
    mov al, ' '
    mov bl, COLOR_BG
    call PutChar
    pop ax
    dec si
    loop EraseBUpLoop
    
    mov al, paddleSpeed
    sub byte ptr [paddleB], al
    sub byte ptr [paddleB+1], al
    sub byte ptr [paddleB+2], al
    sub byte ptr [paddleB+3], al
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 0
DrawBUpLoop:
    mov dh, [paddleB+si]
    mov dl, 77
    push ax
    mov al, CHAR_PADDLE
    mov bl, COLOR_B
    call PutChar
    pop ax
    inc si
    loop DrawBUpLoop
    ret

MoveBDown:
    mov al, paddleSpeed
    mov bl, byte ptr [paddleB+3]
    add bl, al
    cmp bl, 23
    jg NoMove
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 0
EraseBDownLoop:
    mov dh, [paddleB+si]
    mov dl, 77
    push ax
    mov al, ' '
    mov bl, COLOR_BG
    call PutChar
    pop ax
    inc si
    loop EraseBDownLoop
    
    mov al, paddleSpeed
    add byte ptr [paddleB], al
    add byte ptr [paddleB+1], al
    add byte ptr [paddleB+2], al
    add byte ptr [paddleB+3], al
    
    mov cl, paddleSpeed
    xor ch, ch
    mov si, 3
DrawBDownLoop:
    mov dh, [paddleB+si]
    mov dl, 77
    push ax
    mov al, CHAR_PADDLE
    mov bl, COLOR_B
    call PutChar
    pop ax
    dec si
    loop DrawBDownLoop
    ret
ProcessKey ENDP

UpdateBall PROC
    mov dh, ballY
    mov dl, ballX
    mov al, ' '
    mov bl, COLOR_BG
    call PutChar

    mov al, ballX
    mov bl, ballDX
    cmp bl, 0
    jl MoveBallLeft
    add al, ballSpeedH
    jmp UpdateBallX
MoveBallLeft:
    sub al, ballSpeedH
UpdateBallX:
    mov ballX, al

    mov al, ballY
    add al, [ballDY]
    mov ballY, al

    cmp ballY, 1
    jbe BounceVertical
    cmp ballY, 23
    jae BounceVertical
    jmp CheckPaddles

BounceVertical:
    neg byte ptr ballDY
    call Beep
    cmp ballY, 1
    jne CheckBot
    mov ballY, 2
    jmp CheckPaddles
CheckBot:
    mov ballY, 22

CheckPaddles:
    cmp ballX, 3
    jne CheckPaddleB
    mov si, 0
    mov cx, 4
CheckALoop:
    mov al, [paddleA+si]
    cmp al, ballY
    je HitPaddleA
    inc si
    loop CheckALoop
    jmp CheckScore

HitPaddleA:
    mov ballDX, 1
    mov ballX, 4
    call Beep
    jmp DrawNewBall

CheckPaddleB:
    cmp ballX, 76
    jne CheckScore
    mov si, 0
    mov cx, 4
CheckBLoop:
    mov al, [paddleB+si]
    cmp al, ballY
    je HitPaddleB
    inc si
    loop CheckBLoop
    jmp CheckScore

HitPaddleB:
    mov ballDX, -1
    mov ballX, 75
    call Beep
    jmp DrawNewBall

CheckScore:
    cmp ballX, 1
    jbe PointForB
    cmp ballX, 78
    jae PointForA
    jmp DrawNewBall

PointForA:
    inc scoreA
    call DoubleBeep
    call ActivateBall2
    call ResetBall
    call ShowScore
    ret

PointForB:
    inc scoreB
    call DoubleBeep
    call ActivateBall2
    call ResetBall
    call ShowScore
    ret

DrawNewBall:
    mov dh, ballY
    mov dl, ballX
    mov al, CHAR_BALL
    mov bl, COLOR_BALL
    call PutChar
    ret
UpdateBall ENDP

UpdateBall2 PROC
    mov dh, ball2Y
    mov dl, ball2X
    mov al, ' '
    mov bl, COLOR_BG
    call PutChar

    mov al, ball2X
    mov bl, ball2DX
    cmp bl, 0
    jl MoveBall2Left
    add al, ballSpeedH
    jmp UpdateBall2X
MoveBall2Left:
    sub al, ballSpeedH
UpdateBall2X:
    mov ball2X, al

    mov al, ball2Y
    add al, [ball2DY]
    mov ball2Y, al

    cmp ball2Y, 1
    jbe BounceVertical2
    cmp ball2Y, 23
    jae BounceVertical2
    jmp CheckPaddles2

BounceVertical2:
    neg byte ptr ball2DY
    call Beep
    cmp ball2Y, 1
    jne CheckBot2
    mov ball2Y, 2
    jmp CheckPaddles2
CheckBot2:
    mov ball2Y, 22

CheckPaddles2:
    cmp ball2X, 6
    jg CheckPaddleB2
    cmp ball2X, 2
    jl CheckScore2
    
    cmp ball2DX, 0
    jge CheckScore2
    
    mov si, 0
    mov cx, 4
CheckA2Loop:
    mov al, [paddleA+si]
    cmp al, ball2Y
    je HitPaddleA2
    inc si
    loop CheckA2Loop
    jmp CheckScore2

HitPaddleA2:
    mov ball2DX, 1
    mov ball2X, 4
    call Beep
    jmp DrawNewBall2

CheckPaddleB2:
    cmp ball2X, 73
    jl CheckScore2
    cmp ball2X, 77
    jg CheckScore2
    
    cmp ball2DX, 0
    jle CheckScore2
    
    mov si, 0
    mov cx, 4
CheckB2Loop:
    mov al, [paddleB+si]
    cmp al, ball2Y
    je HitPaddleB2
    inc si
    loop CheckB2Loop
    jmp CheckScore2

HitPaddleB2:
    mov ball2DX, -1
    mov ball2X, 75
    call Beep
    jmp DrawNewBall2

CheckScore2:
    cmp ball2X, 1
    jbe PointForB2
    cmp ball2X, 78
    jae PointForA2
    jmp DrawNewBall2

PointForA2:
    inc scoreA
    call DoubleBeep
    call ResetBall2
    call ShowScore
    ret

PointForB2:
    inc scoreB
    call DoubleBeep
    call ResetBall2
    call ShowScore
    ret

DrawNewBall2:
    mov dh, ball2Y
    mov dl, ball2X
    mov al, CHAR_BALL
    mov bl, COLOR_BALL2
    call PutChar
    ret
UpdateBall2 ENDP

ActivateBall2 PROC
    cmp ball2Active, 1
    je AlreadyActive
    
    mov al, scoreA
    cmp al, 3
    jae ActivateIt
    
    mov al, scoreB
    cmp al, 3
    jae ActivateIt
    
    ret

ActivateIt:
    mov ball2Active, 1
    
    mov ah, 2
    mov dh, 1
    mov dl, 33
    mov bh, 0
    int 10h
    
    mov ah, 9
    lea dx, msgBall2
    int 21h
    
    call VictorySound

AlreadyActive:
    ret
ActivateBall2 ENDP

ResetBall PROC
    call ClearScreenProperly
    
    mov ballX, 40
    mov ballY, 12
    mov ballDX, 1
    mov ballDY, 1
    
    call DrawBoard
    ret
ResetBall ENDP

ResetBall2 PROC
    mov ball2X, 40
    mov ball2Y, 15
    mov ball2DX, -1
    mov ball2DY, -1
    ret
ResetBall2 ENDP 


ShowScore PROC
    mov ah, 2
    mov dh, 0
    mov dl, 28
    mov bh, 0
    int 10h    
    
    ; Print "PLAYER A" in COLOR_A using string
    mov si, offset playerAText
    mov bl, COLOR_A
    call PrintColoredString
    
    ; Print space and score in YELLOW
    mov ah, 0Eh
    mov bl, 0Eh          ; Yellow color
    mov al, ' '
    int 10h

    mov al, scoreA
    add al, '0'
    int 10h

    mov al, '-'
    int 10h

    mov al, scoreB
    add al, '0'
    int 10h   
    
    ; Print space
    mov al, ' '
    int 10h
    
    ; Print "PLAYER B" in COLOR_B using string
    mov si, offset playerBText
    mov bl, COLOR_B
    call PrintColoredString
    
    ret
ShowScore ENDP

; New efficient procedure to print colored strings
PrintColoredString PROC
    push ax
    push bx
    push cx
    push dx
    push si
    
    mov ah, 3           ; Get cursor position
    mov bh, 0
    int 10h
    
PrintLoop:
    mov al, [si]        ; Get character
    cmp al, 0           ; Check for null terminator
    je PrintDone
    
    mov ah, 09h         ; Write character with color
    mov cx, 1
    int 10h
    
    inc dl              ; Move cursor right
    mov ah, 2
    int 10h
    
    inc si              ; Next character
    jmp PrintLoop
    
PrintDone:
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
PrintColoredString ENDP  




; -------------------------------------------------------------
; [FEATURE: COLOR GRAPHICS START]
; -------------------------------------------------------------
PutChar PROC
    push ax
    push bx
    push cx
    push dx
    
    mov ah, 2       ; AH register = 2 (Set Cursor Position)
    mov bh, 0       ; BH register = 0 (Page 0)
    int 10h         ; Call BIOS to move cursor to DX (Row/Col)
    
    mov ah, 9       ; AH register = 9 (Write Char + Attribute)
    mov bh, 0       ; BH register = 0 (Page 0)
    mov cx, 1       ; CX register = 1 (Count)
                    ; AL holds Char, BL holds Color (passed from DrawBoard)
    int 10h         ; Call BIOS to write AL with color BL
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
PutChar ENDP
; -------------------------------------------------------------
; [FEATURE: COLOR GRAPHICS END]
; -------------------------------------------------------------

END MAIN        


DrawBoard PROC
    ; --- DRAWING PADDLE A (The Left Player) ---
    
    mov si, 0        ; SI register = 0. We start at the FIRST pixel of the paddle.
                     ; Think: i = 0
                     
    mov cx, 4        ; CX register = 4. This is the LOOP COUNTER.
                     ; We need to draw 4 blocks because the paddle height is 4.

DrawPaddleALoop:
    ; 1. GET THE Y-COORDINATE
    mov dh, [paddleA+si] ; accessing the array.
                         ; IF SI=0: Get value at paddleA (e.g., 10)
                         ; IF SI=1: Get value at paddleA+1 (e.g., 11)
                         ; DH now holds the Row number where we want to draw.

    ; 2. SET THE X-COORDINATE
    mov dl, 2            ; DL register = 2.
                         ; The left paddle is always fixed at Column 2.

    ; 3. SET THE CHARACTER
    mov al, CHAR_PADDLE  ; AL register = 219 (The solid block character '¦')

    ; 4. SET THE COLOR (The "Feature" Part)
    mov bl, COLOR_A      ; BL register gets the value from variable COLOR_A (0Ch = Red).
                         ; BIOS function 9 uses BL for color.

    ; 5. DRAW IT
    call PutChar         ; Jump to the PutChar function to actually put pixels on screen.

    ; 6. PREPARE FOR NEXT PIXEL
    inc si               ; Increment SI (SI = SI + 1).
                         ; Now SI points to the NEXT Y-coordinate in the array.
    
    loop DrawPaddleALoop ; The LOOP instruction does two things:
                         ; a. Dec cx (CX = CX - 1)
                         ; b. If CX is not 0, Jump back to DrawPaddleALoop label.

    ; --- DRAWING PADDLE B (The Right Player) ---
    ; (Logic is identical to above, just different X position and Color)
    
    mov si, 0            ; Reset SI to 0 to start from the top of Paddle B array
    mov cx, 4            ; Reset Loop Counter to 4

DrawPaddleBLoop:
    mov dh, [paddleB+si] ; Get Y-coord from Paddle B array
    mov dl, 77           ; DL = 77. Right paddle is fixed at Column 77.
    mov al, CHAR_PADDLE  ; AL = Block Char
    mov bl, COLOR_B      ; BL = Cyan Color (0Bh) <--- COLOR FEATURE
    call PutChar
    inc si               ; Move to next part of paddle
    loop DrawPaddleBLoop

    ; --- DRAWING THE BALL ---
    
    mov dh, ballY        ; Load Ball Y-pos into DH (Row)
    mov dl, ballX        ; Load Ball X-pos into DL (Col)
    mov al, CHAR_BALL    ; Load Sun character into AL
    mov bl, COLOR_BALL   ; Load Yellow Color (0Eh) into BL <--- COLOR FEATURE
    call PutChar         ; Draw the ball

    ; --- DRAWING BALL 2 (Multi-Ball Feature) ---
    
    cmp ball2Active, 1   ; Check flag: Is the second ball active?
    jne SkipDrawBall2    ; If Not Equal to 1 (Zero Flag=0), Jump over drawing logic.

    mov dh, ball2Y       ; Load Ball 2 Y
    mov dl, ball2X       ; Load Ball 2 X
    mov al, CHAR_BALL    ; Load Sun Character
    mov bl, COLOR_BALL2  ; Load Magenta Color (0Dh) <--- COLOR FEATURE
    call PutChar

SkipDrawBall2:
    call ShowScore       ; Draw the text score at the top
    ret                  ; Return to where DrawBoard was called
DrawBoard ENDP 



PutChar PROC
    ; ---------------------------------------------------------
    ; STEP 1: SAVE REGISTERS (Stack Operations)
    ; ---------------------------------------------------------
    ; Why? This function is called from the middle of other loops (like DrawBoard).
    ; Those loops use AX, BX, CX, DX to count things. If we change them here
    ; without saving them, the main game loop will crash or bug out.
    push ax
    push bx
    push cx
    push dx
    
    ; ---------------------------------------------------------
    ; STEP 2: MOVE THE CURSOR
    ; ---------------------------------------------------------
    mov ah, 2       ; AH = 2 is the BIOS command for "Set Cursor Position".
    mov bh, 0       ; BH = 0 selects "Page 0". (The standard video page).
    ; Note: DH (Row) and DL (Column) are passed into this function by the caller.
    ; We don't set them here; we use whatever values are already in them.
    int 10h         ; INT 10h executes the Video Service.
                    ; RESULT: The blinking cursor jumps to coordinates (DL, DH).
    
    ; ---------------------------------------------------------
    ; STEP 3: WRITE THE CHARACTER & COLOR
    ; ---------------------------------------------------------
    mov ah, 9       ; AH = 9 is the BIOS command for "Write Character & Attribute".
    mov bh, 0       ; BH = 0 selects "Page 0".
    mov cx, 1       ; CX = 1 tells BIOS to write the character only 1 time.
    ; Note: AL (Character) and BL (Color) are passed into this function.
    ; AL holds the ASCII code (like 'O' or a block 219).
    ; BL holds the Color Attribute (like 0Ch for Red).
    int 10h         ; INT 10h executes the Video Service.
                    ; RESULT: The character in AL appears at the cursor location,
                    ; painted with the color defined in BL.
    
    ; ---------------------------------------------------------
    ; STEP 4: RESTORE REGISTERS
    ; ---------------------------------------------------------
    pop dx          ; Restore DX to what it was before we started.
    pop cx          ; Restore CX.
    pop bx          ; Restore BX.
    pop ax          ; Restore AX.
    ; Note: We POP in reverse order (LIFO - Last In, First Out).
    
    ret             ; Return to the main code.
PutChar ENDP   



ShowScore PROC
    ; --- STEP 1: MOVE CURSOR TO START POSITION ---
    mov ah, 2       ; AH = 2 means "Set Cursor Position"
    mov dh, 0       ; DH = Row 0 (Top line of screen)
    mov dl, 28      ; DL = Column 28 (Center-ish horizontally)
    mov bh, 0       ; BH = Page 0 (Standard video page)
    int 10h         ; Call BIOS. Cursor is now at (0, 28).

    ; --- STEP 2: PRINT "PLAYER A" IN RED ---
    ; Logic: We want custom color, so we use our helper function.
    mov si, offset playerAText ; SI register gets the memory address of the string "PLAYER A"
    mov bl, COLOR_A            ; BL register gets the Color Code (0Ch = Red)
    call PrintColoredString    ; Go to helper function (Explained below)

    ; --- STEP 3: PRINT SCORES (SIMPLE METHOD) ---
    ; Logic: For numbers and symbols, we use "Teletype Mode" (AH=0Eh).
    ; It's easier because it auto-advances the cursor after printing.
    ; NOTE: Colors here depend on the emulator's previous setting (often Yellow or White).

    mov ah, 0Eh     ; AH = 0Eh means "Write Character and Advance Cursor"
    mov bl, 0Eh     ; BL = 0Eh (Yellow). Some BIOSes use this for color in Teletype mode.

    ; A. Print Space
    mov al, ' '     ; AL = Space character
    int 10h         ; Print it. Cursor moves right automatically.

    ; B. Print Score A
    mov al, scoreA  ; Load the score number (e.g., 5) into AL
    add al, '0'     ; MATH TRICK: 
                    ; ASCII '0' is 48.
                    ; If Score is 5, then 5 + 48 = 53.
                    ; ASCII 53 is the character '5'.
                    ; We MUST do this to print a number symbol.
    int 10h         ; Print the number.

    ; C. Print Hyphen
    mov al, '-'     ; Load Hyphen char
    int 10h         ; Print it.

    ; D. Print Score B
    mov al, scoreB  ; Load Score B
    add al, '0'     ; Convert to ASCII
    int 10h         ; Print it.
    
    ; E. Print Space
    mov al, ' '     ; Load Space
    int 10h         ; Print it.

    ; --- STEP 4: PRINT "PLAYER B" IN CYAN ---
    mov si, offset playerBText ; SI points to "PLAYER B" string in memory
    mov bl, COLOR_B            ; BL gets Cyan Color (0Bh)
    call PrintColoredString    ; Go to helper function
    
    ret             ; Return to main game loop
ShowScore ENDP