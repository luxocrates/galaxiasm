module Test.BiDirIsaTestCases (BiDirIsaTestCase, biDirIsaTestCases) where

import Data.Word (Word8)

type BiDirIsaTestCase = ([Word8], String);

-- | Bidirectional test cases for the instruction set architecture: the
--   assembler should give exactly the specified bytes when given the specified
--   string, and the disassembler should give exactly the specified string when
--   given the specified bytes.
--
--   Note: the instruction "ld hl,(nn)" could be represented either as
--         [$2a,nn,nn] or as [$ed,$6b,nn,nn]. When assembling, we should always
--         choose the shortest one. Likewise for "ld (nn),hl".
biDirIsaTestCases :: [BiDirIsaTestCase]
biDirIsaTestCases =
  -- 8-bit load
  [ ([0b01111000                          ], "ld   a,b")
  , ([0b00111110, 0xaa                    ], "ld   a,$aa")
  , ([0b01000110                          ], "ld   b,(hl)")
  , ([0b11011101, 0b01111110, 0b01111111  ], "ld   a,(ix+$7f)")
  , ([0b11111101, 0b01111110, 0b01111111  ], "ld   a,(iy+$7f)")
  , ([0b01110111                          ], "ld   (hl),a")
  , ([0b11011101, 0b01110111, 0b00000000  ], "ld   (ix+$00),a")
  , ([0b11111101, 0b01110111, 0b01111110  ], "ld   (iy+$7e),a")
  , ([0b00110110, 0x0                     ], "ld   (hl),$00")
  , ([0xdd, 0x36, 0x12, 0x55              ], "ld   (ix+$12),$55")
  , ([0xfd, 0x36, 0x55, 0x34              ], "ld   (iy+$55),$34")
  , ([0x0a                                ], "ld   a,(bc)")
  , ([0x1a                                ], "ld   a,(de)")
  , ([0x3a, 0x12, 0x34                    ], "ld   a,($3412)")
  , ([0x02                                ], "ld   (bc),a")
  , ([0x12                                ], "ld   (de),a")
  , ([0x32, 0x12, 0x34                    ], "ld   ($3412),a")
  , ([0xed, 0x57                          ], "ld   a,i")
  , ([0xed, 0x5f                          ], "ld   a,r")
  , ([0xed, 0x47                          ], "ld   i,a")
  , ([0xed, 0x4f                          ], "ld   r,a")

  -- 16-bit load
  , ([0x01, 0x12, 0x34                    ], "ld   bc,$3412")
  , ([0xdd, 0x21, 0x12, 0x34              ], "ld   ix,$3412")
  , ([0xfd, 0x21, 0x12, 0x34              ], "ld   iy,$3412")
  , ([0x2a, 0x12, 0x34                    ], "ld   hl,($3412)")  -- see note
  , ([0xed, 0b01011011, 0x12, 0x34        ], "ld   de,($3412)")

  , ([0xdd, 0x2a, 0x12, 0x34              ], "ld   ix,($3412)")
  , ([0xfd, 0x2a, 0x12, 0x34              ], "ld   iy,($3412)")
  , ([0x22, 0x12, 0x34                    ], "ld   ($3412),hl")  -- see note
  , ([0xed, 0b01010011, 0x12, 0x34        ], "ld   ($3412),de")

  , ([0xdd, 0x22, 0x12, 0x34              ], "ld   ($3412),ix")
  , ([0xfd, 0x22, 0x12, 0x34              ], "ld   ($3412),iy")
  , ([0xf9                                ], "ld   sp,hl")
  , ([0xdd, 0xf9                          ], "ld   sp,ix")
  , ([0xfd, 0xf9                          ], "ld   sp,iy")
  , ([0b11110101                          ], "push af")
  , ([0xdd, 0xe5                          ], "push ix")
  , ([0xfd, 0xe5                          ], "push iy")
  , ([0b11000001                          ], "pop  bc")
  , ([0xdd, 0xe1                          ], "pop  ix")
  , ([0xfd, 0xe1                          ], "pop  iy")

  -- Exchange, block transfer, and search group
  , ([0xeb                                ], "ex   de,hl")
  , ([0x08                                ], "ex   af,af'")
  , ([0xd9                                ], "exx")
  , ([0xe3                                ], "ex   (sp),hl")
  , ([0xdd, 0xe3                          ], "ex   (sp),ix")
  , ([0xfd, 0xe3                          ], "ex   (sp),iy")
  , ([0xed, 0xa0                          ], "ldi")
  , ([0xed, 0xb0                          ], "ldir")
  , ([0xed, 0xa8                          ], "ldd")
  , ([0xed, 0xb8                          ], "lddr")
  , ([0xed, 0xa1                          ], "cpi")
  , ([0xed, 0xb1                          ], "cpir")
  , ([0xed, 0xa9                          ], "cpd")
  , ([0xed, 0xb9                          ], "cpdr")

  -- 8-bit arithmetic group
  , ([0b10000111                          ], "add  a,a")
  , ([0xc6, 0xaa                          ], "add  a,$aa")
  , ([0x86                                ], "add  a,(hl)")
  , ([0xdd, 0x86, 0x56                    ], "add  a,(ix+$56)")
  , ([0xfd, 0x86, 0x78                    ], "add  a,(iy+$78)")
  , ([0b10001111                          ], "adc  a,a")
  , ([0xce, 0xaa                          ], "adc  a,$aa")
  , ([0x8e                                ], "adc  a,(hl)")
  , ([0xdd, 0x8e, 0x00                    ], "adc  a,(ix+$00)")
  , ([0xfd, 0x8e, 0x12                    ], "adc  a,(iy+$12)")

  , ([0b10010111                          ], "sub  a")
  , ([0xd6, 0xaa                          ], "sub  $aa")
  , ([0x96                                ], "sub  (hl)")
  , ([0xdd, 0x96, 0x34                    ], "sub  (ix+$34)")
  , ([0xfd, 0x96, 0x55                    ], "sub  (iy+$55)")

  , ([0b10011111                          ], "sbc  a,a")
  , ([0xde, 0xaa                          ], "sbc  a,$aa")
  , ([0x9e                                ], "sbc  a,(hl)")
  , ([0xdd, 0x9e, 0x56                    ], "sbc  a,(ix+$56)")
  , ([0xfd, 0x9e, 0x55                    ], "sbc  a,(iy+$55)")

  , ([0b10100111                          ], "and  a")
  , ([0xe6, 0xaa                          ], "and  $aa")
  , ([0xa6                                ], "and  (hl)")
  , ([0xdd, 0xa6, 0x78                    ], "and  (ix+$78)")
  , ([0xfd, 0xa6, 0x55                    ], "and  (iy+$55)")

  , ([0b10110111                          ], "or   a")
  , ([0xf6, 0xaa                          ], "or   $aa")
  , ([0xb6                                ], "or   (hl)")
  , ([0xdd, 0xb6, 0x01                    ], "or   (ix+$01)")
  , ([0xfd, 0xb6, 0x55                    ], "or   (iy+$55)")

  , ([0b10101111                          ], "xor  a")
  , ([0xee, 0xaa                          ], "xor  $aa")
  , ([0xae                                ], "xor  (hl)")
  , ([0xdd, 0xae, 0x23                    ], "xor  (ix+$23)")
  , ([0xfd, 0xae, 0x55                    ], "xor  (iy+$55)")

  , ([0b10111111                          ], "cp   a")
  , ([0xfe, 0xaa                          ], "cp   $aa")
  , ([0xbe                                ], "cp   (hl)")
  , ([0xdd, 0xbe, 0x45                    ], "cp   (ix+$45)")
  , ([0xfd, 0xbe, 0x55                    ], "cp   (iy+$55)")

  , ([0b00111100                          ], "inc  a")
  , ([0b00110100                          ], "inc  (hl)")
  , ([0xdd, 0x34, 0x67                    ], "inc  (ix+$67)")
  , ([0xfd, 0x34, 0x70                    ], "inc  (iy+$70)")

  , ([0b00111101                          ], "dec  a")
  , ([0b00110101                          ], "dec  (hl)")
  , ([0xdd, 0x35, 0x21                    ], "dec  (ix+$21)")
  , ([0xfd, 0x35, 0x32                    ], "dec  (iy+$32)")

  -- General-purpose arithmetic and CPU control groups
  , ([0x27                                ], "daa")
  , ([0x2f                                ], "cpl")
  , ([0xed, 0x44                          ], "neg")
  , ([0x3f                                ], "ccf")
  , ([0x37                                ], "scf")
  , ([0x00                                ], "nop")
  , ([0x76                                ], "halt")
  , ([0xf3                                ], "di")
  , ([0xfb                                ], "ei")
  , ([0xed, 0x46                          ], "im   0")
  , ([0xed, 0x56                          ], "im   1")
  , ([0xed, 0x5e                          ], "im   2")

  -- 16-bit arithmetic group
  , ([0b00001001                          ], "add  hl,bc")
  , ([0xed, 0b01001010                    ], "adc  hl,bc")
  , ([0xed, 0b01000010                    ], "sbc  hl,bc")
  , ([0xdd, 0b00101001                    ], "add  ix,ix")
  , ([0xfd, 0b00101001                    ], "add  iy,iy")

  , ([0b00100011                          ], "inc  hl")
  , ([0xdd, 0b00100011                    ], "inc  ix")
  , ([0xfd, 0b00100011                    ], "inc  iy")

  , ([0b00101011                          ], "dec  hl")
  , ([0xdd, 0b00101011                    ], "dec  ix")
  , ([0xfd, 0b00101011                    ], "dec  iy")

  -- Rotate and shift group
  , ([0x07                                ], "rlca")
  , ([0x17                                ], "rla")
  , ([0x0f                                ], "rrca")
  , ([0x1f                                ], "rra")

  , ([0xcb, 0b00000111                    ], "rlc  a")
  , ([0xcb, 0x06                          ], "rlc  (hl)")
  , ([0xdd, 0xcb, 0x32, 0x06              ], "rlc  (ix+$32)")
  , ([0xfd, 0xcb, 0x43, 0x06              ], "rlc  (iy+$43)")

  , ([0xcb, 0b00010111                    ], "rl   a")
  , ([0xcb, 0x16                          ], "rl   (hl)")
  , ([0xdd, 0xcb, 0x54, 0x16              ], "rl   (ix+$54)")
  , ([0xfd, 0xcb, 0x65, 0x16              ], "rl   (iy+$65)")

  , ([0xcb, 0b00001111                    ], "rrc  a")
  , ([0xcb, 0x0e                          ], "rrc  (hl)")
  , ([0xdd, 0xcb, 0x76, 0x0e              ], "rrc  (ix+$76)")
  , ([0xfd, 0xcb, 0x07, 0x0e              ], "rrc  (iy+$07)")

  , ([0xcb, 0b00011111                    ], "rr   a")
  , ([0xcb, 0x1e                          ], "rr   (hl)")
  , ([0xdd, 0xcb, 0x10, 0x1e              ], "rr   (ix+$10)")
  , ([0xfd, 0xcb, 0x21, 0x1e              ], "rr   (iy+$21)")

  , ([0xcb, 0b00100111                    ], "sla  a")
  , ([0xcb, 0x26                          ], "sla  (hl)")
  , ([0xdd, 0xcb, 0x32, 0x26              ], "sla  (ix+$32)")
  , ([0xfd, 0xcb, 0x43, 0x26              ], "sla  (iy+$43)")

  , ([0xcb, 0b00101111                    ], "sra  a")
  , ([0xcb, 0x2e                          ], "sra  (hl)")
  , ([0xdd, 0xcb, 0x54, 0x2e              ], "sra  (ix+$54)")
  , ([0xfd, 0xcb, 0x65, 0x2e              ], "sra  (iy+$65)")

  , ([0xcb, 0b00111111                    ], "srl  a")
  , ([0xcb, 0x3e                          ], "srl  (hl)")
  , ([0xdd, 0xcb, 0x76, 0x3e              ], "srl  (ix+$76)")
  , ([0xfd, 0xcb, 0x67, 0x3e              ], "srl  (iy+$67)")

  , ([0xed, 0x6f                          ], "rld")
  , ([0xed, 0x67                          ], "rrd")

  -- Bit set, reset, and test group
  , ([0xcb, 0b01101010                    ], "bit  5,d")
  , ([0xcb, 0b01010110                    ], "bit  2,(hl)")
  , ([0xdd, 0xcb, 0xff, 0b01101110        ], "bit  5,(ix-$01)")
  , ([0xfd, 0xcb, 0xfe, 0b01101110        ], "bit  5,(iy-$02)")

  , ([0xcb, 0b11101010                    ], "set  5,d")
  , ([0xcb, 0b11010110                    ], "set  2,(hl)")
  , ([0xdd, 0xcb, 0x80, 0b11101110        ], "set  5,(ix-$80)")
  , ([0xfd, 0xcb, 0x81, 0b11101110        ], "set  5,(iy-$7f)")

  , ([0xcb, 0b10101010                    ], "res  5,d")
  , ([0xcb, 0b10010110                    ], "res  2,(hl)")
  , ([0xdd, 0xcb, 0x55, 0b10101110        ], "res  5,(ix+$55)")
  , ([0xfd, 0xcb, 0x00, 0b10101110        ], "res  5,(iy+$00)")

  -- Jump group
  , ([0xc3, 0x12, 0x34                    ], "jp   $3412")
  , ([0b11010010, 0x12, 0x34              ], "jp   nc,$3412")
  , ([0x18, 0x80                          ], "jr   $0082")
  , ([0x38, 0x80                          ], "jr   c,$0082")
  , ([0x30, 0x80                          ], "jr   nc,$0082")
  , ([0x28, 0x80                          ], "jr   z,$0082")
  , ([0x20, 0x80                          ], "jr   nz,$0082")
  , ([0xe9                                ], "jp   (hl)")
  , ([0xdd, 0xe9                          ], "jp   (ix)")
  , ([0xfd, 0xe9                          ], "jp   (iy)")
  , ([0x10, 0x80                          ], "djnz $0082")

  -- Call and return group
  , ([0xcd, 0x12, 0x34                    ], "call $3412")
  , ([0b11010100, 0x12, 0x34              ], "call nc,$3412")
  , ([0xc9                                ], "ret")
  , ([0b11010000                          ], "ret  nc")
  , ([0xed, 0x4d                          ], "reti")
  , ([0xed, 0x45                          ], "retn")
  , ([0b11011111                          ], "rst  $18")

  -- Input and output group
  , ([0xdb, 0xaa                          ], "in   a,($aa)")
  , ([0xed, 0b01010000                    ], "in   d,(c)")
  , ([0xed, 0xa2                          ], "ini")
  , ([0xed, 0xb2                          ], "inir")
  , ([0xed, 0xaa                          ], "ind")
  , ([0xed, 0xba                          ], "indr")
  , ([0xd3, 0xaa                          ], "out  ($aa),a")
  , ([0xed, 0b01010001                    ], "out  (c),d")
  , ([0xed, 0xa3                          ], "outi")
  , ([0xed, 0xb3                          ], "otir")
  , ([0xed, 0xab                          ], "outd")
  , ([0xed, 0xbb                          ], "otdr")

  -- All variants of a 'b' parameter
  , ([0xcb, 0b01000101                    ], "bit  0,l")
  , ([0xcb, 0b01001101                    ], "bit  1,l")
  , ([0xcb, 0b01010101                    ], "bit  2,l")
  , ([0xcb, 0b01011101                    ], "bit  3,l")
  , ([0xcb, 0b01100101                    ], "bit  4,l")
  , ([0xcb, 0b01101101                    ], "bit  5,l")
  , ([0xcb, 0b01110101                    ], "bit  6,l")
  , ([0xcb, 0b01111101                    ], "bit  7,l")

  -- Some variants of an 'n' parameter
  , ([0x3e, 0x00                          ], "ld   a,$00")
  , ([0x3e, 0x55                          ], "ld   a,$55")
  , ([0x3e, 0xaa                          ], "ld   a,$aa")
  , ([0x3e, 0xff                          ], "ld   a,$ff")

  -- All variants of a 'p' parameter
  , ([0b11000111                          ], "rst  $00")
  , ([0b11001111                          ], "rst  $08")
  , ([0b11010111                          ], "rst  $10")
  , ([0b11011111                          ], "rst  $18")
  , ([0b11100111                          ], "rst  $20")
  , ([0b11101111                          ], "rst  $28")
  , ([0b11111111                          ], "rst  $38")

  -- All variants of an 'r' parameter
  , ([0b00111110, 0xaa                    ], "ld   a,$aa")
  , ([0b00000110, 0xaa                    ], "ld   b,$aa")
  , ([0b00001110, 0xaa                    ], "ld   c,$aa")
  , ([0b00010110, 0xaa                    ], "ld   d,$aa")
  , ([0b00011110, 0xaa                    ], "ld   e,$aa")
  , ([0b00100110, 0xaa                    ], "ld   h,$aa")
  , ([0b00101110, 0xaa                    ], "ld   l,$aa")

  -- All variants of an 'cc' parameter
  , ([0b11000010, 0xaa, 0x55              ], "jp   nz,$55aa")
  , ([0b11001010, 0xaa, 0x55              ], "jp   z,$55aa")
  , ([0b11010010, 0xaa, 0x55              ], "jp   nc,$55aa")
  , ([0b11011010, 0xaa, 0x55              ], "jp   c,$55aa")
  , ([0b11100010, 0xaa, 0x55              ], "jp   po,$55aa")
  , ([0b11101010, 0xaa, 0x55              ], "jp   pe,$55aa")
  , ([0b11110010, 0xaa, 0x55              ], "jp   p,$55aa")
  , ([0b11111010, 0xaa, 0x55              ], "jp   m,$55aa")

  -- Some variants of a 'dd' parameter
  , ([0x3e, 0x00                          ], "ld   a,$00")
  , ([0x3e, 0x55                          ], "ld   a,$55")
  , ([0x3e, 0xaa                          ], "ld   a,$aa")
  , ([0x3e, 0xff                          ], "ld   a,$ff")

  -- Some variants of an 'nn' parameter
  , ([0x01, 0x00, 0x00                    ], "ld   bc,$0000")
  , ([0x01, 0xa5, 0x5a                    ], "ld   bc,$5aa5")
  , ([0x01, 0x5a, 0xa5                    ], "ld   bc,$a55a")
  , ([0x01, 0xff, 0xff                    ], "ld   bc,$ffff")

  -- All variants of a 'pp' parameter
  , ([0xdd, 0b00001001                    ], "add  ix,bc")
  , ([0xdd, 0b00011001                    ], "add  ix,de")
  , ([0xdd, 0b00101001                    ], "add  ix,ix")
  , ([0xdd, 0b00111001                    ], "add  ix,sp")

  -- All variants of a 'qq' parameter
  , ([0b11000101                          ], "push bc")
  , ([0b11010101                          ], "push de")
  , ([0b11100101                          ], "push hl")
  , ([0b11110101                          ], "push af")

  -- All variants of a 'rr' parameter
  , ([0xfd, 0b00001001                    ], "add  iy,bc")
  , ([0xfd, 0b00011001                    ], "add  iy,de")
  , ([0xfd, 0b00101001                    ], "add  iy,iy")
  , ([0xfd, 0b00111001                    ], "add  iy,sp")

  -- All variants of an 'ss' parameter
  , ([0b00001001                          ], "add  hl,bc")
  , ([0b00011001                          ], "add  hl,de")
  , ([0b00101001                          ], "add  hl,hl")
  , ([0b00111001                          ], "add  hl,sp")
  ]
