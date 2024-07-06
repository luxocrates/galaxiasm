module Test.Pass3 (test) where

import Data.Word (Word8)

import Asm.Pass1 (pass1)
import Asm.Pass2 (pass2)
import Asm.Pass3 (pass3)
import Asm.Types.AsmFailable (AsmError)
import Asm.Types.Pass3 (Pass3Artifacts)
import Test.BiDirIsaTestCases (BiDirIsaTestCase, biDirIsaTestCases)


test :: Bool
test = and
  [ bidirectionalTests
  , ixyPlusDTests
  , symbolTests
  , exprTests
  , directiveTests
  , failureTests
  ]


assembleToPass3 :: String -> Either AsmError Pass3Artifacts
assembleToPass3 program =
      pass1 program "(unit test)"
  >>= pass2
  >>= pass3


-- In our test rig, we're ignoring the error location, if there is one
rig :: String -> Either String [Word8]
rig program =
  case assembleToPass3 program of
    Left (_, details) -> Left $ show details
    Right (bytes, _)  -> Right bytes


bidirectionalTests :: Bool
bidirectionalTests = iter biDirIsaTestCases
  where
    iter :: [BiDirIsaTestCase] -> Bool
    iter []              = True
    iter ((w8s, str):ts) = (rig str == Right w8s) && iter ts


ixyPlusDTests :: Bool
ixyPlusDTests = and
  [ rig "ld d,(ix+0)"      == Right [0xdd, 0b01010110, 0x0]
  , rig "ld d,(ix-0)"      == Right [0xdd, 0b01010110, 0x0]

  , rig "ld d,(ix+127)"    == Right [0xdd, 0b01010110, 0x7f]
  , rig "ld d,(ix+128)"    == Left "BoundsError 128 (-128) 127"

  , rig "ld d,(ix-128)"    == Right [0xdd, 0b01010110, 0x80]
  , rig "ld d,(ix-129)"    == Left "BoundsError (-129) (-128) 127"

  , rig "ld d,(iy+0)"      == Right [0xfd, 0b01010110, 0x0]
  , rig "ld d,(ix+$10)"    == Right [0xdd, 0b01010110, 0x10]

  , rig "ld d,(ix+$10+10)" == Right [0xdd, 0b01010110, 0x1a]
  ]


symbolTests :: Bool
symbolTests = and
  [ rig "sym=12\n.byte sym"     == Right [12]
  , rig "sym=256\n.byte sym"    == Left "BoundsError 256 (-128) 255"
  , rig "sym = 1 + lbl\nlbl:"   == Left "UnavailSymbolError"
  , rig ".word lbl\nlbl:"       == Right [0x02, 0x00]

  , rig "ld a,(_1)"             == Left "LocalUseButNoParentError"
  , rig "_1:"                   == Left "LocalDefButNoParentError"
  , rig "lbl:\nlbl:"            == Left "RedefLabelError \"lbl\""
  , rig "sym=1\nsym=1"          == Left "RedefSymbolError \"sym\""
  , rig "ld a,lbl"              == Left "UndefSymbolError \"lbl\""

    -- local labels
  , rig ".org $1234\nparent:\n_1: .word _1"
                                == Right ((take 0x1234 (repeat 0)) ++ [0x34, 0x12])
  , rig "g1:\n_2:\ng2:.word _2"
                                == Left "UndefSymbolError \"g2_2\""
  ]


exprTests :: Bool
exprTests = and
  [ -- Symbol usage
    rig "sym = $1234\nld bc,sym" == Right [0x01, 0x34, 0x12]
  , rig "sym = 1234\nld bc,sym"  == Right [0x01, 0xd2, 0x04]

    -- Addition, subtraction
  , rig "ld a,12+1"              == Right [0x3e, 13]
  , rig "ld a,12+1+1"            == Right [0x3e, 14]
  , rig "ld a,12-1"              == Right [0x3e, 11]
  , rig "ld a,12+1-1"            == Right [0x3e, 12]

    -- Multiplication
  , rig "ld a,2*4"               == Right [0x3e, 8]
  , rig "ld a,2*4+1"             == Right [0x3e, 9]
  , rig "ld a,1+2*4"             == Right [0x3e, 9]

    -- Bitwise operators
  , rig "ld a,$aa&$55"           == Right [0x3e, 0x00]
  , rig "ld a,$aa|$55"           == Right [0x3e, 0xff]
  , rig "ld a,$ee&$77"           == Right [0x3e, 0x66]
  , rig "ld a,$66|$33"           == Right [0x3e, 0x77]

    -- Highbyte/lowbyte
  , rig ".byte >$1234"           == Right [0x12]
  , rig ".byte <$1234"           == Right [0x34]
  , rig ".byte >$0000+1"         == Right [1]

    -- Unary minus
  , rig ".byte -1"               == Right [0xff]

    -- Parentheses
    -- When the parens aren't on the outside, they're a sub-expression for an
    -- immediate instruction
  , rig "ld a,(1+2)*4"           == Right [0x3e, 12]
  , rig "ld a,1+(2*4)"           == Right [0x3e, 9]
    -- When the parens are on the outside, this becomes an absolute instruction
  , rig "ld a,((1+2)*4)"         == Right [0x3a, 12, 0]

    -- Spacing
  , rig "ld a,3 + ( 5*2 )"       == Right [0x3e, 13]
  ]


directiveTests :: Bool
directiveTests = and
    -- .byte
  [ rig ".byte 1"               == Right [0x01]
  , rig ".byte 1,2"             == Right [0x01, 0x02]
  , rig ".byte 1,2,3"           == Right [0x01, 0x02, 0x03]
  , rig ".byte -1"              == Right [0xff]
  , rig ".byte $ff"             == Right [0xff]
  , rig ".byte $100"            == Left "BoundsError 256 (-128) 255"
  , rig ".byte -128"            == Right [0x80]
  , rig ".byte -129"            == Left "BoundsError (-129) (-128) 255"

    -- .word
  , rig ".word $1234"           == Right [0x34, 0x12]
  , rig ".word $1234,$5678"     == Right [0x34, 0x12, 0x78, 0x56]
  , rig ".word 65535"           == Right [0xff, 0xff]
  , rig ".word 65536"           == Left "BoundsError 65536 (-32768) 65535"
  , rig ".word -32768"          == Right [0x00, 0x80]
  , rig ".word -32769"          == Left "BoundsError (-32769) (-32768) 65535"

    -- .org
  , rig ".org 0\n.byte $ff"     == Right [0xff]
  , rig ".org 3\n.byte $ff"     == Right [0, 0, 0, 0xff]
  , rig ".org 3,-1\n.byte 0"    == Right [0xff, 0xff, 0xff, 0]
  , rig "nop\n.org 0"           == Left "OrgTooSmallError 0 1"
  , rig ".org $10000"           == Left "OrgTooBigError 65536"
  , rig ".org 10,256"           == Left "FillByteError 256"

    -- .align
  , rig ".align 2\n.byte 1"     == Right [1]
  , rig ".byte 9\n.align 2\n.byte 1"
                                == Right [9, 0, 1]
  , rig ".byte 9\n.align 2,7\n.byte 1"
                                == Right [9, 7, 1]
  , rig ".org $ffff\n.align $2" == Left "AlignTooBigError 65536"

    -- .gstring
  , rig ".gstring \"\""         == Right []
  , rig ".gstring \"HELLO\""    == Right [0x48, 0x45, 0x4c, 0x4c, 0x4f]
    -- TODO: test failure cases here

    -- Note: a single .checksumbalance can't be tested here; that can only be
    -- done after pass 4
  , rig ".checksumbalance\n.checksumbalance"
                                == Left "CsBalanceRedefError"
  ]


-- Only exercises the error cases not covered elsewhere
failureTests :: Bool
failureTests = and
  [ rig "rst 27"                == Left "InvalidRstError"
  , rig "a::"                   == Left "SyntaxError"
  , rig ".org $ffff\nnop\nnop"  == Left "AddressError 65536"

  -- The .org $ffff takes way too long to compile. Which is a problem.
  -- , rig ".org $ffff\nnop"       == Right (replicate 0xffff 0) 
  ]
