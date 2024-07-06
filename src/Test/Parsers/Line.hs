module Test.Parsers.Line (test) where

import Asm.CharStream (makeCs)
import Asm.Parsers.Line (parseLine)
import Asm.Types.CharStream (CharStream (CS))
import Asm.Types.Parser (ParseAttempt (ParseSuccess))
import Asm.Types.Pass1
  ( LineArtifact (Instruction, LabelDefinition, SymbolAssignment), PositionedString
  )


test :: Bool
test = and
  [ rig ""                 == []
  , rig " "                == []
  , rig ";"                == []
  , rig "; ff"             == []
  , rig " ; ff"            == []
  , rig " ; ff ; ff"       == []

  , rig "symbol =1234 "    == [ SymbolAssignment (makePositionedString "symbol"
                                "symbol =1234 " 0 6 ) 1234 ]
  , rig "symbol= 1234 "    == [ SymbolAssignment (makePositionedString "symbol"
                                "symbol= 1234 " 0 6 ) 1234 ]
  , rig "symbol = 1234"    == [ SymbolAssignment (makePositionedString "symbol"
                                "symbol = 1234" 0 6 ) 1234 ]
  , rig "symbol = 1234;"   == [ SymbolAssignment (makePositionedString "symbol"
                                "symbol = 1234;" 0 6 ) 1234 ]
  , rig "symbol = 1234 ;"  == [ SymbolAssignment (makePositionedString "symbol"
                                "symbol = 1234 ;" 0 6 ) 1234 ]
  , rig " symbol = 1234 ;" == [ SymbolAssignment (makePositionedString "symbol"
                                " symbol = 1234 ;" 1 7 ) 1234 ]

  , rig "insn"             == [ Instruction (CS "insn"     0 4 ("", 1)) ]
  , rig " insn"            == [ Instruction (CS " insn"    1 5 ("", 1)) ]
  , rig " insn "           == [ Instruction (CS " insn "   1 5 ("", 1)) ]
  , rig " insn;x"          == [ Instruction (CS " insn;x"  1 5 ("", 1)) ]
  , rig " insn ;x"         == [ Instruction (CS " insn ;x" 1 5 ("", 1)) ]

  , rig "in sn"            == [ Instruction (CS "in sn"   0 5 ("", 1)) ]
  , rig " in sn"           == [ Instruction (CS " in sn"  1 6 ("", 1)) ]
  , rig " in sn "          == [ Instruction (CS " in sn " 1 6 ("", 1)) ]

  , rig "label:"           == [ LabelDefinition (makePositionedString "label"
                                "label:"           0 5) ]
  , rig " label: "         == [ LabelDefinition (makePositionedString "label"
                                " label: "         1 6) ]
  , rig " label:;comment"  == [ LabelDefinition (makePositionedString "label"
                                " label:;comment"  1 6) ]
  , rig " label: ;comment" == [ LabelDefinition (makePositionedString "label"
                                " label: ;comment" 1 6) ]
  , rig " label: ;"        == [ LabelDefinition (makePositionedString "label"
                                " label: ;"        1 6) ]

  , rig "ll: insn"         == [ LabelDefinition (makePositionedString "ll" "ll: insn" 0 2)
                              , Instruction (CS "ll: insn" 4 8 ("", 1))
                              ]
  , rig "ll:insn"          == [ LabelDefinition (makePositionedString "ll" "ll:insn" 0 2)
                              , Instruction (CS "ll:insn" 3 7 ("", 1))
                              ]
  , rig "ll:insn "         == [ LabelDefinition (makePositionedString "ll" "ll:insn " 0 2)
                              , Instruction (CS "ll:insn " 3 7 ("", 1))
                              ]
  , rig "ll:insn;comment"  == [ LabelDefinition (makePositionedString "ll" "ll:insn;comment" 0 2)
                              , Instruction (CS "ll:insn;comment" 3 7 ("", 1))
                              ]
  , rig "ll:insn ;comment" == [ LabelDefinition (makePositionedString "ll" "ll:insn ;comment" 0 2)
                              , Instruction (CS "ll:insn ;comment" 3 7 ("",1))
                              ]
  ]


rig :: String -> [LineArtifact]
rig text
  =
    expectParseSuccessWithNoRemainder $
      parseLine $
        makeCs text ("", 1)
  where
    expectParseSuccessWithNoRemainder (ParseSuccess (CS str pos _ _, arts))
      | pos == length str  = arts
      | otherwise          = error "ParseAttempt CharStream had leftovers"
    expectParseSuccessWithNoRemainder _
                           = error "ParseAttempt isn't ParseSuccess"


makePositionedString :: String -> String -> Int -> Int -> PositionedString
makePositionedString string source start end = 
  ( string
  , CS source start len lineOrigin
  , CS source end   len lineOrigin
  )
  where
    lineOrigin = ("", 1)
    len = length source
