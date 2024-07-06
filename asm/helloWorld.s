; helloWorld - a minimal program for Galaxian hardware that prints to the
; screen.
;

; Memory-mapped I/O and other hardware constants
MMIO_VRAM           = $5000   ; (read/write)
MMIO_OBJRAM         = $5800   ; (read/write)
MMIO_COIN_LOCKOUT   = $6002   ; (write) flag: coin mechanism should accept coins
MMIO_NMI_ON         = $7001   ; (write)
MMIO_STARS_ON       = $7004   ; (write)
MMIO_HFLIP          = $7006   ; (write)
MMIO_VFLIP          = $7007   ; (write)
MMIO_WATCHDOG_RESET = $7800   ; (read)
MMIO_PITCH          = $7800   ; (write)

RAM_START           = $4000
RAM_END             = $43FF

  .org $0000                    ; boot address, hardcoded within Z80
COLD_RESET:
  ld   sp,RAM_END+1             ; set up stack pointer
  ld   a,$01
  ld   (MMIO_NMI_ON),a          ; enable interrupts
  jp   MAIN                     ; jump to user code

  .org $0066                    ; hardcoded within Z80
NMI_HANDLER:
  push af                       ; preserve variables
  ld   a,(MMIO_WATCHDOG_RESET)  ; kick the watchdog (by reading, not writing)
  ;
  ; The NMI interrupt handler is triggered exactly once per video frame, and is
  ; effectively the only timing signal the program gets. To animate anything,
  ; you'll need to either put its code here, or raise a signal here that the
  ; main program awaits.
  ;
  xor  a                        ; A = 0
  ld   (MMIO_NMI_ON),a          ; acknowledge interrupt
  inc  a                        ; A = 1
  ld   (MMIO_NMI_ON),a          ; re-enable interrupts
  pop  af                       ; reinstate variables
  ret                           ; done

INIT_HARDWARE:
  ; The tone generator can't be switched off but can be made inaudibly high-pitched.
  ; Do this first to minimize suffering.
  ;
  ld   a,$FF
  ld   (MMIO_PITCH),a

  ; Set graphics mode
  ;
  xor  a                        ; A = 0
  ld   (MMIO_STARS_ON),a        ; stars off
  ld   (MMIO_HFLIP),a           ; don't flip horizontally
  ld   (MMIO_VFLIP),a           ; don't flip vertically

  ; Clear VRAM (tilemap)
  ;
  ld   a,$10                    ; a space character
  ld   hl,MMIO_VRAM             ; HL = start of VRAM
  ld   (hl),a                   ; store the space
  ld   de,MMIO_VRAM+1           ; DE = 1 byte ahead of HL
  ld   bc,$03FF                 ; BC = $3FF bytes to copy
  ldir                          ; copy BC bytes from HL to DE

  ; Clear OBJRAM (sprites, bullets, row columns and row colors)
  ;
  xor  a                        ; value for fill
  ld   hl,MMIO_OBJRAM           ; dest ptr
  ld   b,$80                    ; number of bytes to fill
  call _1

  ; Clear background sound generation ($6004-$6007)
  ;
  ld   hl,$6004                 ; dest ptr
  ld   b,$04                    ; number of bytes to fill
  call _1

  ; Clear sound control ($6800-$6807)
  ;
  ld   hl,$6800                 ; dest ptr
  ld   b,$08                    ; number of bytes to fill
  call _1

  ; Start accepting coins
  ;
  ld   a,1
  ld   (MMIO_COIN_LOCKOUT),a    ; disable the lockout

  ; Hardware's ready
  ret

_1:
  ld   (hl),a                   ; store A at dest
  inc  hl                       ; bump dest pointer
  djnz _1                       ; B--; branch to _1 if still nonzero
  ret                           ; done


; Main program
;
MAIN:
  call INIT_HARDWARE            ; clear the screen, silence the sound
  ld   de,STR                   ; set string source
  ld   hl,MMIO_VRAM+$3A0        ; set screen dest
  call PRINT                    ; print
_1:
  jr   _1                       ; loop forever


PRINT:
  ld   bc,-32                   ; 32 chars (one column)
_1:
  ld   a,(de)                   ; load next character from source
  or   a                        ; is it 0? (set flags)
  ret  z                        ; return if so
  sub  $30                      ; convert from ASCII to Galaxian font encoding
  ld   (hl),a                   ; store at dest
  inc  de                       ; bump source pointer to next character
  add  hl,bc                    ; bump dest pointer to next column
  jr   _1                       ; loop

STR:
  .gstring "HELLO WORLD"
  .byte $00                    ; string termination
