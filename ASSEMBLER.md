# Using the Assembler

## Limitations

The Galaxiasm assembler was written to compile a symbolicated disassembly of the
Galaxian ROMs, and it doesn’t do a whole lot more than that. Notably, it has no
provision for macros.

## Command-line options

For a list of all command-line options, run

    ./galaxiasm --help

Note that the assembler will output 2 KiB ROM files with Galaxian-appropriate
filenames unless it’s run with the `-k` flag.


## Global labels and symbols

    BLINKY = $5000       ; defines symbol BLINKY as having address $5000

    PINKY:               ; defines label PINKY as the current address
      ld a,(BLINKY)      ; load a byte from $5000
      jr PINKY           ; loop

Symbol definitions can be numeric expressions, but can’t reference labels or
other symbols.

## Local labels

Local labels are numbers beginning with an underscore, and act as suffixes for
whichever non-local label was most recently defined.

    INKY:
      ; ...
    _0:              ; defines INKY_0
      jr _0          ; loops to itself
    
    CLYDE:
      ld hl,_0       ; HL := address of `ld` instruction below
    _0:              ; defines CLYDE_0
      ld de,INKY_0   ; DE := address of INKY's jump-to-self instruction

## Numeric expressions

    ld a,1234        ; decimal literal
    ld a,$abcd       ; hex literal
    ld de,TEXT_PTR   ; symbol/label literal
    ld a,1+1         ; addition
    ld a,1-1         ; subtraction
    ld a,1*1         ; multiplication
    ld a,1&1         ; bitwise and
    ld a,1|1         ; bitwise or
    ld l,<TEXT_PTR   ; get low byte of symbol/label
    ld h,>TEXT_PTR   ; get high byte of symbol/label
    ld h,>$0000+1    ; = 1, not 0

## Parentheses

Galaxiasm allows parentheses in numeric expressions, but use them cautiously.
If an instruction offers an absolute mode, outer parentheses will be considered
to be part of the instruction, not part of the expression:

    ld a,(1+2)+4    ; = ld a,7    - an immediate instruction
    ld a,(1+2+4)    ; = ld a,(7)  - an absolute instruction

## Directives

### .db / .byte

    .db $12        ; emits a single byte
    .db 1,2,3      ; emits three bytes

### .dw / .word

    .dw $1234         ; emits $1234 as lowbyte/highbyte ($34, $12)
    .dw $1234,$5678   ; emits $34, $12, $78, $56
    .dw MMIO_VRAM     ; emits a pointer to symbol MMIO_VRAM

### .checksumbalance

    .checksumbalance    ; emits a single byte

The `.checksumbalance` directive causes a byte to be emitted at the current
location whose value is calculated to make the overall program code checksum
equal to zero. This directive is provided in anticipation of compilable
Galaxian sources being included at a later date.

If the file being assembled doesn’t include the Galaxian ROM check, it won’t
need this directive.

`.checksumbalance` can only appear once.

This is _not_ related to the validation that MAME, MiSTer, etc. might perform
when booting the ROMs. There’s nothing that can be done within the code to
mimic the hashes of the original ROMs; you’ll just have to bypass the tests
yourself.

(For MiSTer, change the `md5` attribute of the `<rom>` tag to `md5="none"`)


### .org

    .org $1000       ; make the next instruction start at $1000
                     ; (fill any gap with zeros)

    .org $1000,$ff   ; make the next instruction start at $1000
                     ; (fill any gap with $ff's)


### .align

    .align 8         ; make the next instruction start on an 8-byte boundary
                     ; (fill any gap with zeros)

    .align 8,$ff     ; make the next instruction start on an 8-byte boundary
                     ; (fill any gap with $ff's)


### .gstring

    .gstring "GAME OVER"
                    ; string literal appropriate for how Galaxian uses
                    ; its default font (ASCII-related)
                    ; No sentinel is emitted
                    ; Unsupported characters will cause a syntax error


### .include

    .include "constants.s"
                    ; assumes same path as source file


## Output

The output code will be written to the `out` directory as ROM files suitable
as substitutions for the original `galaxian` set. (Binary files named 
`galmidw.u`, `galmidw.v`, `galmidw.w`, `galmidw.y`, and `7l`).

If the code exceeds 10 KiB, the additional output files will be named
`extra_1`, `extra_2`, etc. Conversely, if the output code is under 8 KiB,
the `7l` file won’t be generated. If it’s under 6 KiB, the `galmidw.y` file
won’t be generated, and so on.


## Testing

    make tests

Runs a suite of unit tests of the assembler and disassembler, exercising all
modes of all Z80 instructions.
