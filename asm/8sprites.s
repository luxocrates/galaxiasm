; There's a widely-believed myth that Galaxian has only seven hardware sprites.
; MAME sources, from which I'm guessing this myth originates, claim (at time of
; writing) that the sprite rendering process runs out of time after just 7.5
; sprites.
;
; This is a demo to debunk that myth, spinning eight full sprites around in a
; circle. Ironically, it renders fine in MAME, as it does on real hardware.
;

; Memory-mapped I/O
MMIO_VRAM           = $5000         ; (read/write) VRAM start address
MMIO_OBJRAM         = $5800         ; (read/write) OBJRAM start address
MMIO_NMI_ON         = $7001         ; (write) enables NMI generation. Must be cleared after interrupts.
MMIO_WATCHDOG_RESET = $7800         ; (read) watchdog reset
MMIO_PITCH          = $7800         ; (write) tone generator PITCH

; Sprite locations and offsets within OBJRAM
MMIO_SPRITES        = $5840
  sprV              = 0
  sprImage          = 1
  sprColor          = 2
  sprH              = 3
  spr_SIZE          = 4
  spr_ENTRIES       = 8

; Default palette PROM colors
PALETTE_0_BLK_BLK_WHT  = 0
PALETTE_1_ORG_BLU_YLW  = 1
PALETTE_2_BLU_RED_YLW  = 2
PALETTE_3_BLU_PUR_RED  = 3
PALETTE_4_BLU_CYN_RED  = 4
PALETTE_5_BLK_BLK_RED  = 5
PALETTE_6_WHT_RED_CYN  = 6
PALETTE_7_YLW_RED_PUR  = 7

; Approximate center of the screen for sprite placement
CENTER_V            = $78
CENTER_H            = $80

; Work RAM
RAM_START           = $4000
TICKS               = $4000
RAM_END             = $43FF


  .org $0000                        ; boot address, hardcoded within Z80
COLD_RESET:
  xor  a
  ld   (MMIO_NMI_ON),a              ; ensure interrupts are disabled
  ld   sp,RAM_END+1                 ; set up stack pointer
  jp   MAIN                         ; start main thread

  .org $0066                        ; hardcoded within Z80
NMI_HANDLER:
  ; no point preserving variables: the main thread is an empty loop
  ld   a,(MMIO_WATCHDOG_RESET)      ; kick the watchdog
  call TICK                         ; jump to main handler
  xor  a
  ld   (MMIO_NMI_ON),a              ; acknowledge interrupt
  inc  a
  ld   (MMIO_NMI_ON),a              ; re-enable interrupts
  ret                               ; done

; Main program
;
MAIN:
  call INIT_HARDWARE                ; clear screen, silence sound
  call SET_SPRITE_IMAGES_AND_COLORS ; set up the static parts of the sprite descriptions
  ld   a,1
  ld   (MMIO_NMI_ON),a              ; enable interrupts

LOOP:
  jr   LOOP                         ; repeat forever

; Ultra-lazy, slow, shotgun approach to hardware initialization: fill everything
; from $5000 onwards with zero, then silence the tone generator.
INIT_HARDWARE:
  xor  a
  ld   (MMIO_NMI_ON),a              ; disable interrupts while we set things up

  ld   hl,$5000                     ; point HL to start address for wipe
_1:
  xor  a                            ; we'll be writing zero
_2:
  ld   (hl),a                       ; store at destination
  inc  l                            ; bump destination pointer low byte
  jr   nz,_2                        ; repeat until L wraps around

  ld   a,(MMIO_WATCHDOG_RESET)      ; kick the watchdog, since we disabled NMI and this is long
  inc  h                            ; bump destination pointer high byte
  jr   nz,_1                        ; repeat until H wraps around

  ld   a,$FF
  ld   (MMIO_PITCH),a               ; silence the tone generator

  ; Clear VRAM (tilemap)
  ;
  ld   a,$10                        ; a space character
  ld   hl,MMIO_VRAM                 ; HL = start of VRAM
  ld   (hl),a                       ; store the space
  ld   de,MMIO_VRAM+1               ; DE = 1 byte ahead of HL
  ld   bc,$03FF                     ; BC = $3FF bytes to copy
  ldir                              ; copy BC bytes from HL to DE

  ret

; Called once per tick (in VBLANK) by the NMI handler
;
TICK:
  ; Increment the frame counter
  ld   a,(TICKS)                    ; load the ticks counter
  inc  a                            ; bump it
  ld   (TICKS),a                    ; store it back

  ; Draw all the sprites
  ld   b,spr_ENTRIES                ; 8 sprites to process
  ld   ix,MMIO_SPRITES              ; IX = start of hardware sprites array
  ld   de,spr_SIZE                  ; length of hardware sprite structure
_1:
  ld   a,b                          ; sprite number (proxy) to A
  rlca                              ; multiply..
  rlca
  rlca
  rlca                              ; ..by 16 (1/8th of the sine table)
  ld   c,a                          ; stash it in C

  ; Look up and write the sprite V co-ordinate
  ld   h,>SINE_TABLE                ; H = high byte of sine table (which is page-aligned)
  ld   a,(TICKS)                    ; A = frame counter
  add  a,c                          ; add sprite number * 1/8th of the table index
  and  $7F                          ; mask to sine table size
  ld   l,a                          ; L is now the correct low byte for table access
  ld   a,(hl)                       ; look up table value; it's centered around 0
  add  a,CENTER_V                   ; center it around the screen's V center
  ld   (ix+sprV),a                  ; store in sprite V value

  ; Look up and write the sprite H co-ordinate.
  ; We ought to nudge this by one pixel for certain sprites, but we're being
  ; lazy. You won't notice the difference.
  ld   h,>SINE_TABLE                ; H = high byte of sine table (which is page-aligned)
  ld   a,(TICKS)                    ; A = frame counter
  add  a,c                          ; add sprite number * 1/8th of the table index
  add  a,$20                        ; add another 90 degrees to make this a cosine lookup
  and  $7F                          ; mask to sine table size
  ld   l,a                          ; L is now the correct low byte for table access
  ld   a,(hl)                       ; look up table value; it's centered around 0
  add  a,CENTER_H                   ; center it around the screen's H center
  ld   (ix+sprH),a                  ; store in sprite H value

  add  ix,de                        ; bump IX to next sprite
  djnz _1                           ; repeat for all sprites

  ; All done
  ret

SET_SPRITE_IMAGES_AND_COLORS:
  ld   hl,_2                        ; source data
  ld   ix,MMIO_SPRITES              ; start of hardware sprites array
  ld   b,spr_ENTRIES                ; 8 sprites to set up
  ld   de,spr_SIZE                  ; 4 bytes between sprite structs
_1:
  ld   a,(hl)                       ; load next source byte (image)
  inc  hl                           ; bump source pointer
  ld   (ix+sprImage),a              ; store image
  ld   a,(hl)                       ; load next source byte (color)
  inc  hl                           ; bump source pointer
  ld   (ix+sprColor),a              ; store color
  add  ix,de                        ; bump IX to next sprite struct
  djnz _1                           ; repeat for all sprites
  ret
_2:
  .byte $11,PALETTE_2_BLU_RED_YLW   ; insect
  .byte $19,PALETTE_6_WHT_RED_CYN   ; galaxip
  .byte $1A,PALETTE_6_WHT_RED_CYN   ; 10 flag
  .byte $1F,PALETTE_7_YLW_RED_PUR   ; explosion
  .byte $23,PALETTE_2_BLU_RED_YLW   ; 800
  .byte $29,PALETTE_1_ORG_BLU_YLW   ; flagship
  .byte $20,PALETTE_7_YLW_RED_PUR   ; 150
  .byte $18,PALETTE_4_BLU_CYN_RED   ; galaxip with outline


; A complete sine wave, over 128 steps, centered around zero, with wavelength 160
.align $100
SINE_TABLE:
  .byte $00, $03, $07, $0B, $0F, $13, $17, $1A
  .byte $1E, $22, $25, $29, $2C, $2F, $32, $35
  .byte $38, $3B, $3D, $40, $42, $44, $46, $48
  .byte $49, $4B, $4C, $4D, $4E, $4F, $4F, $4F
  .byte $50, $4F, $4F, $4F, $4E, $4D, $4C, $4B
  .byte $49, $48, $46, $44, $42, $40, $3D, $3B
  .byte $38, $35, $32, $2F, $2C, $29, $25, $22
  .byte $1E, $1A, $17, $13, $0F, $0B, $07, $03
  .byte $00, $FD, $F9, $F5, $F1, $ED, $E9, $E6
  .byte $E2, $DE, $DB, $D7, $D4, $D1, $CE, $CB
  .byte $C8, $C5, $C3, $C0, $BE, $BC, $BA, $B8
  .byte $B7, $B5, $B4, $B3, $B2, $B1, $B1, $B1
  .byte $B0, $B1, $B1, $B1, $B2, $B3, $B4, $B5
  .byte $B7, $B8, $BA, $BC, $BE, $C0, $C3, $C5
  .byte $C8, $CB, $CE, $D1, $D4, $D7, $DB, $DE
  .byte $E2, $E6, $E9, $ED, $F1, $F5, $F9, $FD
