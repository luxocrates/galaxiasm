; Memory map explorer - a tool for experimenting how the memory map
; behaves, and for testing real hardware. Fits on a single 2716 ROM.

; Defaults (see also DEFAULT_TABLE label)
DEFAULT_TARGET      = $7004         ; set up the default for turning on the..
DEFAULT_STAGED      = $01           ; ..starfield generator

; Constants
HEX_COLUMN          = 16            ; column (H) for the target/staged hex
TARGET_ROW          = 18            ; row (v) for the target hex
STAGED_ROW          = 12            ; row (v) for the staged hex

; Memory-mapped I/O
MMIO_VRAM           = $5000         ; (read/write) VRAM start address
MMIO_OBJRAM         = $5800         ; (read/write) OBJRAM start address
MMIO_COIN_LOCKOUT   = $6002         ; (write) coin lockout
MMIO_NMI_ON         = $7001         ; (write) enables NMI generation. Must be cleared after interrupts.
MMIO_WATCHDOG_RESET = $7800         ; (read) watchdog reset
MMIO_PITCH          = $7800         ; (write) tone generator PITCH
MMIO_SW0            = $6000         ; (read) coin, p1 control, TEST & SERVICE state
MMIO_SW1            = $6800         ; (read) start button, p2 control, dipsw 1/2 state

MMIO_9N_STARS_ON    = $7004         ; (write) flag: enable starfield generator

; Sprite locations and offsets within OBJRAM
SPRITE1             = $5840
SPRITE2             = $5844
  sprV              = 0
  sprImage          = 1
  sprPalette        = 2
  sprH              = 3

; Standard Galaxian PROM color palettes
PALETTE_0_BLK_BLK_WHT  = 0
PALETTE_1_ORG_BLU_YLW  = 1
PALETTE_2_BLU_RED_YLW  = 2
PALETTE_3_BLU_PUR_RED  = 3
PALETTE_4_BLU_CYN_RED  = 4
PALETTE_5_BLK_BLK_RED  = 5
PALETTE_6_WHT_RED_CYN  = 6
PALETTE_7_YLW_RED_PUR  = 7

; Bit mapping within switches
SW0_P1_SHOOT        = $10
SW0_P1_LEFT         = $04
SW0_P1_RIGHT        = $08
SW0_COIN1           = $01
SW0_COIN2           = $02
SW1_1P_START        = $01


; Work RAM
RAM_START           = $4000
TARGET              = $4000         ; lo/hi for target address
STAGED              = $4002         ; staged value for byte we'll be writing
TABLE               = $4003         ; pointer (lo/hi) for interactivity state table
THIS_SW0            = $4005         ; most recent value read from MMIO_SW0
PREV_SW0            = $4006         ; MMIO_SW0, as read on the previous tick
TICKS               = $4007         ; wall clock
MOVE_MODE           = $4008         ; 1 = selecting digits; 0 = modifying a digit
ANIM                = $4009         ; pointer
CURR                = $400B         ; current byte read from (TARGET)
RAM_END             = $43FF


  .org $0000                        ; hardcoded within Z80
COLD_RESET:
  ld   sp,RAM_END+1                 ; set up stack pointer
  ld   a,1
  ld   (MMIO_NMI_ON),a              ; enable interrupts
  jp   MAIN                         ; jump to user code

  .org $0066                        ; hardcoded within Z80
NMI_HANDLER:
  push af                           ; preserve variables
  push bc
  push de
  push hl
  push ix

  ld   a,(MMIO_WATCHDOG_RESET)      ; kick the watchdog

  call DRAW_SPRITES                 ; do all sprite updates while we're still in VBLANK
  call PROCESS_INPUT                ; jump to main handler

  xor  a
  ld   (MMIO_NMI_ON),a              ; acknowledge interrupt
  inc  a
  ld   (MMIO_NMI_ON),a              ; re-enable interrupts
  pop  ix                           ; reinstate variables
  pop  hl
  pop  de
  pop  bc
  pop  af                      
  ret                               ; done

; Main program
;
MAIN:
  call INIT_HARDWARE                ; clear screen, silence sound, set state

  ; Set initial state
  ld   a,1
  ld   (MOVE_MODE),a                ; default to digit-select mode
  ld   hl,MOVING_ANIM
  ld   (ANIM),hl                    ; set appropriate animation for mode

  ; Draw and colorize the on-screen instructions
  call SET_LINE_PALETTES            ; set palette for each line of text
  ld   de,SCREEN_TEXT               ; set text source
  ld   hl,MMIO_VRAM+(29*$20)+0      ; set VRAM destination
  call PRINT_LINES                  ; fill the screen with text

  ; Set state variables to defaults
  ld   hl,DEFAULT_TARGET
  ld   (TARGET),hl                  ; set default TARGET address
  ld   hl,DEFAULT_STAGED
  ld   (STAGED),hl                  ; set default STAGED byte
  ld   hl,DEFAULT_TABLE
  ld   (TABLE),hl                   ; set default interactivity state

; The main loop just re-prints the target address, staged byte, and current
; value forever (the Z80 has to do _something_, and remember not to update
; OBJRAM values outside of the VBLANK)
;
LOOP:
  ; print target address high-byte
  ld   a,(TARGET+1)
  ld   hl, MMIO_VRAM+($20*TARGET_ROW+HEX_COLUMN)
  call PRINT_HEX

  ; print target address low-byte
  ld   a,(TARGET+0)
  ld   hl,MMIO_VRAM+($20*TARGET_ROW-$20*2+HEX_COLUMN)
  call PRINT_HEX

  ; print staged byte
  ld   a,(STAGED)
  ld   hl,MMIO_VRAM+($20*STAGED_ROW+HEX_COLUMN)
  call PRINT_HEX

  ; print current value of target address
  ld   a,(CURR)
  ld   hl,MMIO_VRAM+($20*STAGED_ROW+HEX_COLUMN)+3
  call PRINT_HEX

  jr   LOOP                         ; repeat forever

; Ultra-lazy, slow, shotgun approach to hardware initialization: fill everything
; from $5000 onwards with zero, then silence the tone generator.
INIT_HARDWARE:
  xor  a
  ld   (MMIO_NMI_ON),a              ; disable interrupts while we set things up

  ld   hl,$5000                     ; point HL to start address for wipe
_1:
  ld   a,0                          ; we'll be writing zero
_2:
  ld   (hl),a                       ; store at destination
  inc  l                            ; bump destination pointer low-byte
  jr   nz,_2                        ; repeat until L wraps around

  ld   a,(MMIO_WATCHDOG_RESET)      ; kick the watchdog, since we disabled NMI and this is long
  inc  h                            ; bump destination pointer high-byte
  jr   nz,_1                        ; repeat until H wraps around

  ld   a,$1
  ld   (MMIO_NMI_ON),a              ; re-enable interrupts

  ld   a,$FF
  ld   (MMIO_PITCH),a               ; silence the tone generator

  ld   a,1
  ld   (MMIO_COIN_LOCKOUT),a        ; accept coins (MAME won't show coin inputs otherwise)
  ret

; Called once per tick (in VBLANK) by the NMI handler
;
DRAW_SPRITES:
  ; Increment the frame counter
  ld   a,(TICKS)                    ; load the ticks counter
  inc  a                            ; bump it
  ld   (TICKS),a                    ; store it back

  ; Animate the cursor sprites
  ld   hl,ANIM                      ; ANIM points to a table with eight sprite definition pointers
  ld   e,(hl)                       ; E = table low-byte
  inc  hl
  ld   d,(hl)                       ; D = table high-byte
  ex   de,hl                        ; HL = animation table pointer
  
  ; Frame counter is still in A. We'll advance an animation frame once every four video frames.
  rrca                              ; halve the frame counter
  and  $0E                          ; make it cycle 0, 2, 4, 6, 8, 10, 12, 14 every other increment
  ld   d,0                          ; offset high-byte = 0
  ld   e,a                          ; offset low-byte = those slowly-cycling even numbers
  add  hl,de                        ; add offset to animation table base address

  ld   e,(hl)                       ; get low-byte of sprite definition from table
  inc  hl
  ld   d,(hl)                       ; get high-byte
  ex   de,hl                        ; swap sprite definition pointer into HL for source pointer
  ld   de,SPRITE1                   ; destination pointer: first hardware sprite
  ld   bc,8                         ; byte count: two hardware sprites' worth
  ldir                              ; copy

  ; Set the sprites' V-coordinate based on the cursor definition, which the interactivity
  ; state table will tell us
  ld   hl,(TABLE)                   ; point HL to start of interactivity table
  ld   a,(hl)                       ; load sprite V pos from the top of the table
  ld   b,a                          ; save it in B
  ld   a,(SPRITE1+sprV)             ; read sprite 1's V-pos, written (as offset) by the animation
  add  a,b                          ; add the two together
  ld   (SPRITE1+sprV),a             ; write to first sprite's V pos
  ld   (SPRITE2+sprV),a             ; write to second sprite's V pos
  ret

PROCESS_INPUT:
  ld   a,(MMIO_SW0)                 ; load SW0 inputs
  ld   (THIS_SW0),a                 ; preserve in THIS_SW0

  and  SW0_P1_SHOOT                 ; isolate P1_SHOOT bit
  jr   z,_1                         ; if not pressed, skip to _1
  ld   a,(PREV_SW0)                 ; consider input from last tick
  and  SW0_P1_SHOOT                 ; was button already pressed then?
  call z,SWITCH_MODE                ; if not, switch mode
_1:
  ld   a,(THIS_SW0)                 ; retrieve SW0 inputs
  and  SW0_P1_LEFT                  ; isolate P1_LEFT bit
  jr   z,_2                         ; if not pressed, skip to _2
  ld   a,(PREV_SW0)                 ; consider input from last tick
  and  SW0_P1_LEFT                  ; was button already pressed then?
  call z,HANDLE_LEFT                ; if not, call handler
_2:
  ld   a,(THIS_SW0)                 ; retrieve SW0 inputs
  and  SW0_P1_RIGHT                 ; isolate P1_RIGHT bit
  jr   z,_3                         ; if not pressed, skip to _3
  ld   a,(PREV_SW0)                 ; consider input from last tick
  and  SW0_P1_RIGHT                 ; was button already pressed then?
  call z,HANDLE_RIGHT               ; if not, call handler

_3:
  ld   ix,TARGET                    ; point IX to TARGET
  ld   l,(ix+0)                     ; load low-byte of address into L
  ld   h,(ix+1)                     ; load high-byte of address into H

  ld   a,(MMIO_SW1)                 ; load SW1 inputs
  and  SW1_1P_START                 ; isolate 1P START button
  jr   z,_4                         ; skip to _4 if it's not pressed
  ld   a,(STAGED)                   ; load STAGED value into A
  ld   (hl),a                       ; write STAGED to (TARGET)

_4:
  ; Read the current value of the TARGET address (it's already in HL). It's
  ; important that we do it here and not in the main thread because TARGET
  ; might point to OBJRAM, in which case we don't want to be reading it outside
  ; of the VBLANK.
  ;
  ld   a,(hl)
  ld   (CURR),a

  ; ...and if the coin input is held, move that read value to the STAGED value
  ld   a,(THIS_SW0)                 ; load SW0 inputs
  and  SW0_COIN1                    ; isolate coin 1 input
  jr   z,_5                         ; skip to _5 if it's not high
  ld   a,(CURR)                     ; bring that read value back
  ld   (STAGED),a                   ; store it in staged

_5:
  ; Remember the switch inputs this tick so we can make comparisons next tick
  ld   a,(THIS_SW0)                 ; move the SW0 value we saw at the start..
  ld   (PREV_SW0),a                 ; ..into the PREV_SW0 value for next tick

  ; All done
  ret


SWITCH_MODE:
  ld   a,(MOVE_MODE)                ; load the mode flag
  cpl                               ; flip all bits
  and  1                            ; mask it to just a flag
  ld   (MOVE_MODE),a                ; store it back
  ld   hl,MOVING_ANIM               ; HL = pointer to the animation table. Assume it's for moving
  jr   nz,_0                        ; in which case, skip to _0
  ld   hl,ADJUSTING_ANIM            ; but if it was for adjusting, point HL to the other animation table
_0:
  ld   (ANIM),hl                    ; store the animation table
  ret


HANDLE_LEFT:
  ld   ix,(TABLE)                   ; point IX to interactivity table
  ld   a,(MOVE_MODE)                ; is the interactivity mode..
  and  a                            ; ..the nibble-select mode?
  jp   nz,LOAD_PREV_TABLE           ; if not, replace TABLE

  ld   h,(ix+4)                     ; load left-button handler high-byte
  ld   l,(ix+3)                     ; load left-button handler low-byte
  jp   CALL_HANDLER                 ; call the handler

HANDLE_RIGHT:
  ld   ix,(TABLE)                   ; point IX to interactivity table
  ld   a,(MOVE_MODE)                ; is the interactivity mode..
  and  a                            ; ..the nibble-select mode?
  jp   nz,LOAD_NEXT_TABLE           ; if not, replace TABLE

  ld   h,(ix+6)                     ; load right-button handler high-byte
  ld   l,(ix+5)                     ; load right-button handler low-byte
  jp   CALL_HANDLER                 ; call the handler


LOAD_NEXT_TABLE:
  ld   ix,(TABLE)                   ; point IX to current table
  ld   h,(ix+8)                     ; H = high-byte of next table
  ld   l,(ix+7)                     ; L = low-byte of next table
  ld   (TABLE),hl                   ; install next table to TABLE
  ret

LOAD_PREV_TABLE:
  ld   ix,(TABLE)                   ; point IX to current table
  ld   h,(ix+10)                    ; H = high-byte of previous table
  ld   l,(ix+9)                     ; L = low-byte of previous table
  ld   (TABLE),hl                   ; install previous table to TABLE
  ret


; Installs a set of 32 column palette values to OBJRAM
; (So ought to be done during VBLANK; we're taking liberties)
;
SET_LINE_PALETTES:
  ld   hl,SCREEN_PALETTES           ; point HL to source table
  ld   de,MMIO_OBJRAM+1             ; point DE to first column palette value
  ld   b,32                         ; remaining columns = 32
_0:
  ld   a,(hl)                       ; load the palette value
  ld   (de),a                       ; store it in OBJRAM
  inc  de                           ; bump the OBJRAM pointer..
  inc  de                           ; ..twice, since there's a scroll value in between
  inc  hl                           ; bump the source table pointer
  djnz _0                           ; loop while --B > 0
  ret

; Prints text to the screen in 28-character lines
;
; Expects:
;   DE = text source
;   HL = VRAM destination
;
PRINT_LINES:
  ld   b,28                         ; 28 chars per line
_1:
  ld   a,(de)                       ; load next character from source
  cp   0                            ; is it 0?
  ret  z                            ; if so, return
  sub  $30                          ; convert from ASCII to Galaxian font encoding
  ld   (hl),a                       ; store in VRAM
  inc  de                           ; bump source pointer to next character
  push bc                           ; stash BC; we need a 16-bit reg pair for math
  ld   bc,-32                       ; BC = 32 chars
  add  hl,bc                        ; bump dest pointer to next column (wrt. player)
  pop  bc                           ; retrieve stashed line counter
  djnz _1                           ; loop while --B > 0

; That's one row (wrt. player) done. Keep iterating.
  ld   bc,(28*$20)+1                ; = 28 chars left (wrt. player), one down
  add  hl,bc                        ; update VRAM pointer
  jr   PRINT_LINES                  ; loop


SCREEN_TEXT:
  .gstring "                            "
  .gstring "GALAXIAN MEMORY MAP EXPLORER"
  .gstring "                            "
  .gstring "                            "
  .gstring "  A TOOL FOR EXPERIMENTING  "
  .gstring "   WITH HARDWARE BEHAVIOR   "
  .gstring "                            "
  .gstring "P1 START WILL WRITE THE BYTE"
  .gstring "IN RED TO THE ADDRESS IN RED"
  .gstring "                            "
  .gstring "  THE DEFAULT ENABLES THE   "
  .gstring "    STARFIELD GENERATOR     "
  .gstring "  PRESS P1 START TO TRY IT  "
  .gstring "                            "
  .gstring "                            "
  .gstring "                            "
  .gstring "                            "
  .gstring "                            "
  .gstring "                            "
  .gstring "    READ VALUE              "
  .gstring "- WRITE PURPOSE MAY DIFFER -"
  .gstring "                            "
  .gstring "                            "
  .gstring "  LEFT-RIGHT - MOVE         "
  .gstring "       SHOOT - CHANGE MODE  "
  .gstring "       START - WRITE MEM    "
  .gstring "        COIN - READ MEM     "
  .gstring "                            "
  .gstring "                            "
  .gstring "            GITHUB DOT COM  "
  .gstring "          SLASH LUXOCRATES  "
  .gstring "                            "
  .db 0


SCREEN_PALETTES:
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_1_ORG_BLU_YLW
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_3_BLU_PUR_RED
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_6_WHT_RED_CYN
  .db PALETTE_6_WHT_RED_CYN
  .db PALETTE_6_WHT_RED_CYN
  .db PALETTE_6_WHT_RED_CYN
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_0_BLK_BLK_WHT
  .db PALETTE_7_YLW_RED_PUR
  .db PALETTE_7_YLW_RED_PUR
  .db PALETTE_0_BLK_BLK_WHT


; Prints a byte to the screen as a 2-digit hex value
;
; Expects:
;   A  = byte to display
;   HL = VRAM address for rightmost digit
;
PRINT_HEX:
  ld   d,a                          ; preserve source
  and  $0F                          ; get least sig. nibble
  ld   (hl),a                       ; write to VRAM
  ld   bc,32                        ; offset for one column (wrt. player)
  add  hl,bc                        ; bump VRAM pointer
  ld   a,d                          ; retrieve saved source
  srl  a                            ; shift right..
  srl  a
  srl  a
  srl  a                            ; ..4 bits
  ld   (hl),a                       ; write to VRAM
  ret


; Common setup for calling DEC_HI_NIBBLE, INC_LO_NIBBLE, etc. 
; Loads a pointer to the subject of the handler into DE;
; loads its value into A, then calls the handler
;
; Expects:
; IX - pointer to current TABLE
; HL - handler for interaction (DEC_HI_NIBBLE, etc.)
;
CALL_HANDLER:
  ld   d,(ix+2)
  ld   e,(ix+1)                     ; DE now has target addr
  ld   a,(de)                       ; load subject value
  jp   (hl)                         ; call handler

DEC_HI_NIBBLE:
  sub  $10                          ; decrement high nibble
  ld   (de),a                       ; store it back
  ret

INC_HI_NIBBLE:
  add  a,$10                        ; increment high nibble
  ld   (de),a                       ; store it back
  ret

DEC_LO_NIBBLE:
  ld   b,a                          ; stash subject value in B
  and  $F0                          ; isolate the upper nibble..
  ld   c,a                          ; ..and store that in C
  ld   a,b                          ; retrieve the original
  and  $0F                          ; isolate lower nibble
  dec  a                            ; decrement it
  and  $0F                          ; mask the result to just the lower nibble
  or   c                            ; bring upper nibble back in
  ld   (de),a                       ; store it back
  ret

INC_LO_NIBBLE:
  ld   b,a                          ; stash subject value in B
  and  $F0                          ; isolate the upper nibble..
  ld   c,a                          ; ..and store that in C
  ld   a,b                          ; retrieve the original
  and  $0F                          ; isolate lower nibble
  inc  a                            ; increment it
  and  $0F                          ; mask the result to just the lower nibble
  or   c                            ; bring upper nibble back in
  ld   (de),a                       ; store it back
  ret


TARGET_NIBBLE_0_TABLE:
  .db -TARGET_ROW*8 + 0*8 + 237     ; sprite V          (+0)
  .dw TARGET+1                      ; byte to affect    (+1)
  .dw DEC_HI_NIBBLE                 ; left handler      (+3)
  .dw INC_HI_NIBBLE                 ; right handler     (+5)
  .dw TARGET_NIBBLE_1_TABLE         ; next table        (+7)
  .dw STAGED_NIBBLE_1_TABLE         ; previous table    (+9)

TARGET_NIBBLE_1_TABLE:
  .db -TARGET_ROW*8 + 1*8 + 237     ; sprite V          (+0)
  .dw TARGET+1                      ; byte to affect    (+1)
  .dw DEC_LO_NIBBLE                 ; left handler      (+3)
  .dw INC_LO_NIBBLE                 ; right handler     (+5)
  .dw TARGET_NIBBLE_2_TABLE         ; next table        (+7)
  .dw TARGET_NIBBLE_0_TABLE         ; previous table    (+9)

TARGET_NIBBLE_2_TABLE:
  .db -TARGET_ROW*8 + 2*8 + 237     ; sprite V          (+0)
  .dw TARGET                        ; byte to affect    (+1)
  .dw DEC_HI_NIBBLE                 ; left handler      (+3)
  .dw INC_HI_NIBBLE                 ; right handler     (+5)
  .dw TARGET_NIBBLE_3_TABLE         ; next table        (+7)
  .dw TARGET_NIBBLE_1_TABLE         ; previous table    (+9)

TARGET_NIBBLE_3_TABLE:
  .db -TARGET_ROW*8 + 3*8 + 237     ; sprite V          (+0)
  .dw TARGET                        ; byte to affect    (+1)
  .dw DEC_LO_NIBBLE                 ; left handler      (+3)
  .dw INC_LO_NIBBLE                 ; right handler     (+5)
  .dw STAGED_NIBBLE_0_TABLE         ; next table        (+7)
  .dw TARGET_NIBBLE_2_TABLE         ; previous table    (+9)

STAGED_NIBBLE_0_TABLE:
  .db -STAGED_ROW*8 + 0*8 + 237     ; sprite V          (+0)
  .dw STAGED                        ; byte to affect    (+1)
  .dw DEC_HI_NIBBLE                 ; left handler      (+3)
  .dw INC_HI_NIBBLE                 ; right handler     (+5)
  .dw STAGED_NIBBLE_1_TABLE         ; next table        (+7)
  .dw TARGET_NIBBLE_3_TABLE         ; previous table    (+9)

DEFAULT_TABLE:
STAGED_NIBBLE_1_TABLE:
  .db -STAGED_ROW*8 + 1*8 + 237     ; sprite V          (+0)
  .dw STAGED                        ; byte to affect    (+1)
  .dw DEC_LO_NIBBLE                 ; left handler      (+3)
  .dw INC_LO_NIBBLE                 ; right handler     (+5)
  .dw TARGET_NIBBLE_0_TABLE         ; next table        (+7)
  .dw STAGED_NIBBLE_0_TABLE         ; previous table    (+9)


; Animations: a set of 8 pointers to sprite definitions, to be cycled through
;

; Digit-select sprites: an insect hovers left and right
MOVING_ANIM:
  .dw MOVING_SPRITES_1
  .dw MOVING_SPRITES_2
  .dw MOVING_SPRITES_3
  .dw MOVING_SPRITES_4
  .dw MOVING_SPRITES_5
  .dw MOVING_SPRITES_6
  .dw MOVING_SPRITES_7
  .dw MOVING_SPRITES_8

; Nibble-adjust sprites: two flagships move up and down, mirroring each other
ADJUSTING_ANIM:
  .dw ADJUSTING_SPRITES_1
  .dw ADJUSTING_SPRITES_2
  .dw ADJUSTING_SPRITES_3
  .dw ADJUSTING_SPRITES_2
  .dw ADJUSTING_SPRITES_1
  .dw ADJUSTING_SPRITES_2
  .dw ADJUSTING_SPRITES_3
  .dw ADJUSTING_SPRITES_2

; Sprite definitions: eight-byte tables to copy directly to the OBJRAM sprite tables
;

; Digit-select sprites: we use H=0 to keep the second sprite offscreen
;
MOVING_SPRITES_1:
  .db $00,$52,$04,HEX_COLUMN*8-15                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h

MOVING_SPRITES_2:
  .db $01,$52,$04,HEX_COLUMN*8-16                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h

MOVING_SPRITES_3:
  .db $01,$51,$04,HEX_COLUMN*8-16                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h

MOVING_SPRITES_4:
  .db $01,$D2,$04,HEX_COLUMN*8-16                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h

MOVING_SPRITES_5:
  .db $00,$D2,$04,HEX_COLUMN*8-15                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h

MOVING_SPRITES_6:
  .db $ff,$D2,$04,HEX_COLUMN*8-16                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h

MOVING_SPRITES_7:
  .db $ff,$51,$04,HEX_COLUMN*8-16                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h

MOVING_SPRITES_8:
  .db $ff,$52,$04,HEX_COLUMN*8-16                   ; SPRITE1 v, image, palette, h
  .db $00,$00,$00,$00                               ; SPRITE2 v, image, palette, h


ADJUSTING_SPRITES_1:
  .db $00,$69,PALETTE_1_ORG_BLU_YLW,HEX_COLUMN*8-16 ; SPRITE1 v, image, palette, h
  .db $00,$29,PALETTE_1_ORG_BLU_YLW,HEX_COLUMN*8+5  ; SPRITE1 v, image, palette, h

ADJUSTING_SPRITES_2:
  .db $00,$69,PALETTE_1_ORG_BLU_YLW,HEX_COLUMN*8-17 ; SPRITE1 v, image, palette, h
  .db $00,$29,PALETTE_1_ORG_BLU_YLW,HEX_COLUMN*8+6  ; SPRITE1 v, image, palette, h

ADJUSTING_SPRITES_3:
  .db $00,$69,PALETTE_1_ORG_BLU_YLW,HEX_COLUMN*8-18 ; SPRITE1 v, image, palette, h
  .db $00,$29,PALETTE_1_ORG_BLU_YLW,HEX_COLUMN*8+7  ; SPRITE1 v, image, palette, h
