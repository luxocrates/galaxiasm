; showFont - draws the whole font in a 16x16 square and animates through the
; color palettes
;

; Memory-mapped I/O and other hardware constants
MMIO_NMI_ON         = $7001   ; (write)
MMIO_STARS_ON       = $7004   ; (write)
MMIO_HFLIP          = $7006   ; (write)
MMIO_VFLIP          = $7007   ; (write)
MMIO_WATCHDOG_RESET = $7800   ; (read)
MMIO_PITCH          = $7800   ; (write)
RAM_END             = $4400

CURRENT_COLOR       = $4000

  .org $0000                  ; boot address, hardcoded within Z80
COLD_RESET:
  ld sp,RAM_END               ; set up stack pointer
  ld a,$01
  ld (MMIO_NMI_ON),a          ; enable interrupts
  jp MAIN                     ; jump to user code

  .org $0066                  ; hardcoded within Z80
NMI_HANDLER:
  push af                     ; preserve variables
  push bc
  push de
  push hl
  ld   a,(MMIO_WATCHDOG_RESET)  ; kick the watchdog (by reading, not writing)
  
  ld   a,(CURRENT_COLOR)
  inc  a
  ld   (CURRENT_COLOR),a
  rrca
  rrca
  rrca
  call SET_COLORS

  xor  a                       ; A := 0
  ld   (MMIO_NMI_ON),a         ; acknowledge interrupt
  inc  a                       ; A := 1
  ld   (MMIO_NMI_ON),a         ; re-enable interrupts
  pop  hl
  pop  de
  pop  bc
  pop  af                     ; reinstate variables
  ret                         ; done

INIT_HARDWARE:
  ; Set graphics mode
  ;
  xor a                       ; A := 0
  ld (MMIO_STARS_ON),a        ; stars off
  ld (MMIO_HFLIP),a           ; don't flip horizontally
  ld (MMIO_VFLIP),a           ; don't flip vertically

  ; Clear VRAM (tilemap)
  ;
  ld a,$10                    ; a space character
  ld hl,$5000                 ; HL = start of VRAM
  ld (hl),a                   ; store the space
  ld de,$5001                 ; DE = 1 byte ahead of HL
  ld bc,$03FF                 ; BC = $3FF bytes to copy
  ldir                        ; copy BC bytes from HL to DE

  ; Clear OBJRAM (sprites, bullets, row columns and row colors)
  ;
  xor a                       ; value for fill
  ld hl,$5800                 ; dest ptr
  ld b,$80                    ; number of bytes to fill
  call _1

  ; Clear background sound generation ($6004-$6007)
  ;
  ld hl,$6004                 ; dest ptr
  ld b,$04                    ; number of bytes to fill
  call _1

  ; Clear sound control ($6800-$6807)
  ;
  ld hl,$6800                 ; dest ptr
  ld b,$08                    ; number of bytes to fill
  call _1

  ; The tone generator can't be switched off but can be made inaudibly high-pitched
  ;
  ld a,$FF
  ld (MMIO_PITCH),a

  ; Hardware's ready
  ret

_1:
  ld (hl),a                   ; store A at dest
  inc hl                      ; bump dest pointer
  djnz _1                     ; B--; branch to _1 if still nonzero
  ret                         ; done


; Main program
;
MAIN:
  call INIT_HARDWARE          ; clear the screen, silence the sound
  
  xor  a                      ; clear A
  ld   b,a                    ; next char to be printed = 0
  ld   c,a                    ; # chars printed this row = 0
  ld   hl,$52E8               ; screen ptr
  ld   de,$FFE0               ; amount to add to screen ptr each time

_1:
  ld   a,b                    ; A <- next char
  ld   (hl),a                 ; store on screen
  add  hl,de                  ; bump screen ptr
  inc  b                      ; bump next char
  inc  c                      ; bump chars drawn this row
  ld   a,b                    ; A <- next char
  cp   $00                    ; feed zero flag from next char code
  jp   z,_2                   ; zero? we're done
  ld   a,c                    ; A <- chars printed this row
  cp   $10                    ; have we done 16?
  jr   nz,_1                  ; if not, loop

  ld   de,$0201               ; offset to start of next row
  add  hl,de                  ; add to screen pointer
  ld   de,$FFE0               ; restore offset
  ld   c,$00                  ; clear # chars printed this row
  jr   _1                     ; resume loop

_2:
  jp  _2                      ; loop forever


SET_COLORS:
  ld   b,$20                  ; do 32 columns
  ld   hl,$5801               ; pointer to first column color
_1:
  ld   (hl),a                 ; store color
  inc  hl                     ; bump pointer...
  inc  hl                     ; ...to next color
  djnz _1                     ; loop
  ret
