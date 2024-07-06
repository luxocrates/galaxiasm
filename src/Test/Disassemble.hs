module Test.Disassemble (test) where

import Disasm.Disassemble (disassemble)
import Test.BiDirIsaTestCases (BiDirIsaTestCase, biDirIsaTestCases)


test
  = foldl
      (\acc (w8s, str)
        -> acc
        && (disassemble w8s == ("$0000: " ++ str ++ "\n"))
      )
      True
      testCases
  where
    testCases :: [BiDirIsaTestCase]
    testCases = biDirIsaTestCases ++ disasmOnly


disasmOnly :: [BiDirIsaTestCase]
disasmOnly =
  -- slow cases
  [ ([0xed, 0b01101011, 0x12, 0x34], "ld   hl,($3412)")
  , ([0xed, 0b01100011, 0x12, 0x34], "ld   ($3412),hl")

  -- non-opcodes
  , ([0xcb, 0x30],                   ".byte $cb\n$0001: .byte $30")
  ]
