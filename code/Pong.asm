
	processor 6502
        include "vcs.h"
        include "macro.h"
        include "xmacro.h"

	seg.u Variables
	org  $80

P0YPos          byte
P1YPos          byte
Score0          byte;
Score1          byte;
BallXPos        byte
BallYPos        byte
Temp            byte
PlayerSpritePtr word;
MissileXPos      byte;
MissileYPos      byte;


yball          byte;
xball          byte;
yballvel       byte;
xballvel       byte;

 
FontBuf	ds 10	; 2x5 array of playfield bytes

; Constants
_BackgroundColor   equ $00 
_PlayfieldColor    equ $00
_P0Color           equ $48  
_P1Color           equ $a8 
_P0ScoreColor      equ $1C 
_P1ScoreColor      equ $00
_BallColor         equ $00
_P0XPos            equ $0A
_P1XPos            equ $80
_PlayerHeight      equ $25
_ScoreDigitsHeight equ $00
_TopBorderYPos     equ 139  ;$4E
_BottomBorderYPos  equ $04

_MissileXPos        equ 70
_Missile2XPos       equ 69

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare MACRO to check if we should render the missile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #0                ; start accumualtor with 0 (null position)
        cpx MissileYPos       ; compare X/scanline with missile y-position
        bne .SkipMissileDraw  ; if is not equal, skip the draw of missile0
        inc MissileYPos       ; else, increase y-position of the bullet/ball
        lda #%00000010        ; and set ENABL second bit to enable missile
.SkipMissileDraw
        sta ENAM0             ; store correct value in the TIA missile register
    ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enable ball if it is on this scanline (in X register)
; Modifies A.
	MAC DRAW_BALL
	lda #%00000000
	cpx yball
        bne .noball
        lda #%00000010	; for ENAM0 the 2nd bit is enable
.noball
	sta ENABL	; enable ball
        ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	seg Code
        org $f000

; Initialize and set initial offsets of objects.
Start	
        CLEAN_START
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #60
    sta P0YPos              
    sta P1YPos
       
    lda #76
    sta yball
    lda #0
    sta xball
    
    ; Set ball initial velocity
    lda #1
    sta yballvel
    lda #$30
    sta xballvel
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
NextFrame
    VERTICAL_SYNC
    TIMER_SETUP 37 ; Wait for VBLANK
            
    lda #_P0XPos
    ldy #0
    jsr SetObjHorizPos        ; set player0 horizontal position (1 WSYNC)
    
    lda #_P1XPos
    ldy #1
    jsr SetObjHorizPos        ; set player1 horizontal position (1 WSYNC)    
          
    lda xball
    ldy #4
    jsr SetObjHorizPos
    sta WSYNC
    sta HMOVE
                    
    lda Score0       
    ldx #0
    jsr GetBCDBitmap
    lda Score1
    ldx #5
    jsr GetBCDBitmap
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed pre-VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #$19
    sta Score0
    lda #$42
    sta Score1
    
;Underscan

         
        TIMER_WAIT
	TIMER_SETUP 20
      
       
        lda #%00010010	; score mode + 2 pixel ball
        sta CTRLPF
        lda #$48
        sta COLUP0	; set color for left
        lda #$a8
        sta COLUP1	; set color for right
; Now we draw all four digits.
	ldy #0		; Y will contain the frame Y coordinate
ScanLoop1
	sta WSYNC
        tya
        lsr		; divide Y by two for double-height lines
        tax		; -> X
        lda FontBuf+0,x
        sta PF1		; set left score bitmap
        SLEEP 28
        lda FontBuf+5,x
        sta PF1		; set right score bitmap
        iny
        cpy #10
        ;sta WSYNC
        bcc ScanLoop1

; Clear the playfield
        lda #0
	sta WSYNC
        sta PF1
; Turn playfield reflection off (and turn score mode off)
        lda #%00010100	; no reflection + ball priority + 2 pixel ball
        sta CTRLPF


        TIMER_WAIT ; 21 scanlines
        
        TIMER_SETUP 174
;------------------------------------------------------------        
        ;TIMER_SETUP 7  
        
       lda #%0000011	; mirror playfield
       sta CTRLPF
       lda #$1C                 ; Yellow playfiled color
       sta COLUPF

       lda #$ff
       sta PF0
       sta PF1
       sta PF2 
       
       ; Turn off drawing players
       ;lda #0
       ;sta GRP0
       ;sta GRP1
       
       ldx #7    
ScanLoop2
       sta WSYNC
       dex
       bne ScanLoop2
       ;TIMER_WAIT ; 7 scanlines
;------------------------------------------------------------        
       
       
;------------------------------------------------------------        
       ;TIMER_SETUP 158 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; 2-line kernel     ;;;;;;;;;;;;;;;;;;;;;;;;;;;    
       lda #%00010000
       sta PF0
       lda #$00
       sta PF1
       lda #%10000000
       sta PF2

       
       ; enable missle
       lda #%00000000        ; and set ENABL second bit to enable missile
       sta ENAM0
       sta ENAM1

       ldx #158
ScanLoop3

; check for ball
;---------------------------------------------------------------------------
;---------------------DRAWING PLAYERS--------------------------------
.IsInsideSpriteP0Check:      ; check if should render sprite player0
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set
    sbc P0YPos               ; subtract sprite Y coordinate
    cmp #_PlayerHeight          ; are we inside the sprite height bounds?
    lda 1
    bcc .DrawSpriteP0        ; if result < SpriteHeight, call subroutine
    lda #0                   ; else, set lookup index to 0
.DrawSpriteP0:
    clc                      ; clears carry flag before addition
    tay                      ; load Y so we can work with pointer
    lda PlayerSprite,Y  ; load player bitmap slice of data
    
    sta WSYNC
    sta GRP0                 ; set graphics for player 0
    lda #_P0Color             ; load player color from lookup table
    sta COLUP0               ; set color for player 0 slice;
.IsInsideSpriteP1Check:      ; check if should render sprite player0
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set
    sbc P1YPos               ; subtract sprite Y coordinate
    cmp #_PlayerHeight          ; are we inside the sprite height bounds?
    lda 1
    bcc .DrawSpriteP1        ; if result < SpriteHeight, call subroutine
    lda #0                   ; else, set lookup index to 0
.DrawSpriteP1:
    clc                      ; clears carry flag before addition
    tay                      ; load Y so we can work with pointer
    lda PlayerSprite,Y  ; load player bitmap slice of data
     sta WSYNC
    sta GRP1                 ; set graphics for player 0
    lda #_P1Color             ; load player color from lookup table
    sta COLUP1               ; set color for player 0 slice;
    
    ;DRAW_BALL
    lda #%00000000
	cpx yball
        bne .noball
        lda #%00000010	; for ENAM0 the 2nd bit is enable
.noball
 sta ENABL
    
    

;----------------------------------------------------------------------------
       dex
       dex
       bne ScanLoop3
       sta WSYNC
       ;TIMER_WAIT ; 158
;------------------------------------------------------------        
       
       
;------------------------------------------------------------        
       ;TIMER_SETUP 8
       lda #$1C                 ; Yellow playfiled color
       sta COLUPF
       lda #$ff
       sta PF0
       sta PF1
       sta PF2
        
        ; disable missle
       lda #%00000000        ; and set ENABL second bit to enable missile
       sta ENAM0
       sta ENAM1  
       
       ; Turn off drawing players
       lda #0
       sta GRP0
       sta GRP1
       
      
       
       
       ldx #7
ScanLoop4
	sta WSYNC
        dex
        bne ScanLoop4
       
       ;TIMER_WAIT ; 8
;------------------------------------------------------------        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
        ; clear pf
        lda #$00
        sta PF0
        sta PF1
        sta PF2
        
        TIMER_WAIT ; wait for 192 scanlines
        
        TIMER_SETUP 28 ; Overscan
        
           dec yball
           inc xball
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process joystick input for player0 up/down/left/right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000           ; player 0 joystick up
    bit SWCHA
    bne CheckP0Down
    lda #_TopBorderYPos
    sbc P0YPos
    bcc CheckP0Down
    inc P0YPos
    inc P0YPos
    lda #0

CheckP0Down:
    lda #%00100000           ; player 0 joystick down
    bit SWCHA
    bne CheckP1Up
    lda #_BottomBorderYPos
    sbc P0YPos               ; compare Y Pos with 0
    bcs CheckP1Up        ; do not change if Y == 0  s
    dec P0YPos
    dec P0YPos
    lda #0

CheckP1Up:
    lda #%00000001           ; player 1 joystick up
    bit SWCHA
    bne CheckP1Down
    lda #_TopBorderYPos
    sbc P1YPos
    bcc CheckP1Down
    inc P1YPos
    inc P1YPos
    lda #0
    
CheckP1Down:
    lda #%00000010           ; player 1 joystick down
    bit SWCHA
    bne EndInputCheck
    lda #_BottomBorderYPos
    sbc P1YPos               ; compare Y Pos with 0
    bcs EndInputCheck        ; do not change if Y == 0  
    dec P1YPos
    dec P1YPos
    lda #0

EndInputCheck:               ; fallback when no input was performed


 

               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        TIMER_WAIT ; Wait for overscan
        jmp NextFrame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
;-----------------------------------------------------------------------------    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjHorizPos subroutine
    sta WSYNC                ; start a fresh new scanline
    sec                      ; make sure carry-flag is set before subtracion
.Div15Loop
    sbc #15                  ; subtract 15 from accumulator
    bcs .Div15Loop           ; loop until carry-flag is clear
    eor #7                   ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                      ; four shift lefts to get only the top 4 bits
    sta HMP0,Y               ; store the fine offset to the correct HMxx
    sta RESP0,Y              ; fix object position in 15-step increment
    rts	; return to caller 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DigitsHex:
    .byte $22,$22,$22,$22,$22
    .byte $EE,$22,$EE,$88,$EE
    .byte $EE,$22,$66,$22,$EE
    .byte $AA,$AA,$EE,$22,$22
    .byte $EE,$88,$EE,$22,$EE
    .byte $EE,$88,$EE,$AA,$EE
    .byte $EE,$22,$22,$22,$22
    .byte $EE,$AA,$EE,$AA,$EE
    .byte $EE,$AA,$EE,$22,$EE

PlayerSprite:
    .byte #%00000000         ;
    .byte #%01110000         ; ##
    
GetBCDBitmap subroutine
; First fetch the bytes for the 1st digit
	pha		; save original BCD number
        and #$0F	; mask out the least significant digit
        sta Temp
        asl
        asl
        adc Temp	; multiply by 5
        tay		; -> Y
        lda #5
        sta Temp	; count down from 5
.loop1
        lda DigitsBitmap,y
        and #$0F	; mask out leftmost digit
        sta FontBuf,x	; store leftmost digit
        iny
        inx
        dec Temp
        bne .loop1
; Now do the 2nd digit
        pla		; restore original BCD number
        lsr
        lsr
        lsr
        lsr		; shift right by 4 (in BCD, divide by 10)
        sta Temp
        asl
        asl
        adc Temp	; multiply by 5
        tay		; -> Y
        dex
        dex
        dex
        dex
        dex		; subtract 5 from X (reset to original)
        lda #5
        sta Temp	; count down from 5
.loop2
        lda DigitsBitmap,y
        and #$F0	; mask out leftmost digit
        ora FontBuf,x	; combine left and right digits
        sta FontBuf,x	; store combined digits
        iny
        inx
        dec Temp
        bne .loop2
	rts

	org $FF00

; Bitmap pattern for digits
DigitsBitmap ;;{w:8,h:5,count:10,brev:1};;
        .byte $EE,$AA,$AA,$AA,$EE
        .byte $22,$22,$22,$22,$22
        .byte $EE,$22,$EE,$88,$EE
        .byte $EE,$22,$66,$22,$EE
        .byte $AA,$AA,$EE,$22,$22
        .byte $EE,$88,$EE,$22,$EE
        .byte $EE,$88,$EE,$AA,$EE
        .byte $EE,$22,$22,$22,$22
        .byte $EE,$AA,$EE,$AA,$EE
        .byte $EE,$AA,$EE,$22,$EE
;;end
 
 
 
 
 
;;
; Epilogue
	org $fffc
        .word Start
        .word Start
