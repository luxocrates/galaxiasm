module Test.Pass2 (test) where

import Data.Either (isLeft)
import Data.Map (toList)

import Asm.Pass1 (pass1)
import Asm.Pass2 (pass2)
import Asm.Types.AsmFailable (AsmFailable)
import Asm.Types.Pass2 (Pass2Artifacts, SymbolType (Equ, Label))


rig :: String -> AsmFailable Pass2Artifacts
rig program =
      pass1 program "(unit test)"
  >>= pass2


extractSymbolTable :: String -> [(String, Int, SymbolType)]
extractSymbolTable program =
  case rig program of
    Left _ -> error "Program unexpectedly failed"
    Right (syms, _, _) ->
      map
        toTuple
        (toList syms)
  where toTuple (label, (value, (typ, _))) = (label, value, typ)


expectFailure :: String -> Bool
expectFailure program = isLeft $ rig program


test :: Bool
test = and
  [
    -- An empty program
    extractSymbolTable ""
      == []

  -- A label definition
  , extractSymbolTable "label:"
      == [("label", 0, Label)]

  -- Multiple label definitions
  , extractSymbolTable "label1:\nlabel2:"
      == [ ("label1", 0, Label)
         , ("label2", 0, Label)
         ]

  -- A symbol definition
  , extractSymbolTable "label = 29"
      == [("label", 29, Equ)]

  -- Multiple symbol definitions
  , extractSymbolTable "label1 = 29\nlabel2 = 92"
      == [ ("label1", 29, Equ)
         , ("label2", 92, Equ)
         ]

    -- A local label definitions
  , extractSymbolTable "label:\n_1:"
      == [ ("label",   0, Label)
         , ("label_1", 0, Label)
         ]

    -- Can't declare a local label when there's no parent
  , expectFailure "_1:"

    -- Can't redeclare a label
  , expectFailure "label:\nlabel:"

    -- Can't redeclare a label as a symbol
  , expectFailure "label:\nlabel = 9"

    -- Can't redeclare a symbol as a label
  , expectFailure "label = 9\nlabel:"

    -- Can't redeclare a local label
  , expectFailure "label:\n_1:\n_1:"

    -- A label after an opcode
  , extractSymbolTable "ld a,a\nlabel:"
      == [("label",  1, Label)]
  ]
