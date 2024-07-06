module Asm.Parsers.Opcode (parseOpcode) where

import Data.Bits (shiftL)
import Data.List (sortBy)
import Data.Word (Word32)

import Asm.ArtifactAccumulator (___, use, used0, used1, used2, used3)
import Asm.BaseParser (parseSuccessFromCs)
import Asm.Parsers.Expr (parseExpr)
import Asm.Parsers.Meta (parseAny, parseKeyword, parseKeywordMap)
import Asm.Parsers.Tokens
  ( parseCloseParens
  , parseComma
  , parseEndOfLine
  , parseMandatorySpace
  , parseOpenParens
  , parseOptionalSpace
  )
import Asm.Types.Parser (ParseAttempt (ParseNearly, ParseSuccess), Parser)
import Asm.Types.Pass2
  ( Ast (..)
  , Ast_PlusMinus (..)
  , OpcodeOrDirective
  , UnresolvedBits
  )
import InstructionSet (Component (..), InstructionDef, instructionSet)


-- | Builds a parser for a single instruction, by building parsers for each of
--   the Z80 instructions defined in the common ISA structure
parseOpcode :: Parser OpcodeOrDirective
parseOpcode =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseAny $ map makeParser preprocessedInstructionSet)
    >>= used1 (\chunks -> (const $ Right (chunks, False)))


-- | Converts an instruction pattern to a left-aligned bit sequence, the
--   variable bits becoming zeros
shuntPatToMsb :: String -> Word32
shuntPatToMsb pat
  = shiftL (patToBinary pat) (32 - length pat)


-- | Converts an instruction definition into a corresponding parser
makeParser :: InstructionDef -> Parser [UnresolvedBits]

-- ...for a zero-operand instruction, eg. `ldir`
makeParser (pat, (comp:[])) =
  \cs -> (parseSuccessFromCs cs)
      >>= ___ (parseComponent comp)
      >>= ___ (parseEndOfLine)
      >>= used0 (\_ ->
            [( (shuntPatToMsb pat)
            , (div (length pat) 8)
            , []
            )]
          )

-- ...for a single-operand instruction, eg. `dec (hl)`
makeParser (pat, (comp1:comp2:[])) =
  \cs -> parseSuccessFromCs cs
      >>= ___ (parseComponent comp1)
      >>= ___ (parseMandatorySpace)
      >>= use (parseComponent comp2)
      >>= ___ (parseEndOfLine)
      >>= used1 (\(arg1) ->
            [( (shuntPatToMsb pat)
            , (div (length pat) 8)
            , arg1
            )])

-- ...for a double-operand instruction, eg. `ld a,(hl)`
makeParser (pat, (comp1:comp2:comp3:[])) =
  \cs -> parseSuccessFromCs cs
      >>= ___ (parseComponent comp1)
      >>= ___ (parseMandatorySpace)
      >>= use (parseComponent comp2)
      >>= ___ (parseOptionalSpace)
      >>= ___ (parseComma)
      >>= ___ (parseOptionalSpace)
      >>= use (parseComponent comp3)
      >>= ___ (parseEndOfLine)
      >>= used2 (\(arg1, arg2) ->
            [( (shuntPatToMsb pat)
            , (div (length pat) 8)
            , arg1 ++ arg2
            )])

-- Shouldn't happen: the maximum arity is 2. We're just placating the compiler.
makeParser _ = \cs -> ParseNearly cs


-- | Convert a pattern string to its binary equivalent, any variable bits
--   becoming zeros
patToBinary :: String -> Word32
patToBinary [] = 0
patToBinary cs = (if last cs == '1' then 1 else 0)
                 + (2 * patToBinary (init cs))


-- | Parse any instruction component
parseComponent :: Component -> Parser [Ast]

-- A keyword
parseComponent (K str) = parseKeyword str []

-- A keyword in parentheses
parseComponent (Parens_K str) = makeParseParensParser (parseKeyword str [])

-- A 'b' parameter
parseComponent (Var_b start) =
  \cs -> parseExpr cs
    >>= \(cs', expr) -> ParseSuccess (cs', [Ast_B start expr])

-- An 'n' parameter
parseComponent (Var_n start) =
  \cs -> parseExpr cs
    >>= \(cs', expr) -> ParseSuccess (cs', [Ast_N start expr])

-- A 'p' parameter
parseComponent (Var_p start) =
  \cs -> parseExpr cs
    >>= \(cs', expr) -> ParseSuccess (cs', [Ast_P start expr])

-- An 'r' parameter (see manual p71)
parseComponent (Var_r start) = parseKeywordMap $ mapTable start
    3
    [ ("A", 0b111)
    , ("B", 0b000)
    , ("C", 0b001)
    , ("D", 0b010)
    , ("E", 0b011)
    , ("H", 0b100)
    , ("L", 0b101)
    ]

-- An IX or IY parameter
parseComponent (Var_ixy start) = parseKeywordMap $ mapTable start
    1
    [ ("ix", 0b0)
    , ("iy", 0b1)
    ]

-- A 'cc' parameter (see manual p263)
parseComponent (Var_cc start) = parseKeywordMap $ mapTable start
    3
    [ ("nz", 0b000)
    , ("z",  0b001)
    , ("nc", 0b010)
    , ("c",  0b011)
    , ("po", 0b100)
    , ("pe", 0b101)
    , ("p",  0b110)
    , ("m",  0b111)
    ]

-- A 'dd' parameter (see manual p99)
-- Surprisingly, the manual's 'Instruction Notation Summary' (Table 4, p39)
-- doesn't mention dd. It is to a destination what 'ss' is to a source.
parseComponent (Var_dd start) = parseKeywordMap $ mapTable start
    2
    [ ("bc", 0b00)
    , ("de", 0b01)
    , ("hl", 0b10)
    , ("sp", 0b11)
    ]

-- An 'nn' parameter (see manual p99)
parseComponent (Var_nn start) =
  \cs -> parseExpr cs
    >>= \(cs', expr) -> ParseSuccess (cs', [Ast_Nn start expr])

-- A 'pp' parameter (see manual p194)
parseComponent (Var_pp start) = parseKeywordMap $ mapTable start
    2
    [ ("bc", 0b00)
    , ("de", 0b01)
    , ("ix", 0b10)
    , ("sp", 0b11)
    ]

-- A 'qq' parameter (see manual p115)
parseComponent (Var_qq start) = parseKeywordMap $ mapTable start
    2
    [ ("bc", 0b00)
    , ("de", 0b01)
    , ("hl", 0b10)
    , ("af", 0b11)
    ]

-- An 'rr' parameter (see manual p196)
parseComponent (Var_rr start) = parseKeywordMap $ mapTable start
    2
    [ ("bc", 0b00)
    , ("de", 0b01)
    , ("iy", 0b10)
    , ("sp", 0b11)
    ]

-- An 'ss' parameter (see manual p188)
parseComponent (Var_ss start) = parseComponent (Var_dd start)

-- An 'e-2' parameter
parseComponent (Var_eMinus2 start) =
  \cs -> parseExpr cs
    >>= \(cs', expr) -> ParseSuccess (cs', [Ast_EMinus2 start expr])

-- An IX or IY parameter, in parentheses
parseComponent (Parens_Var_ixy start) =
  makeParseParensParser $
  parseKeywordMap $
  mapTable start
    1
    [ ("ix", 0b0)
    , ("iy", 0b1)
    ]

-- An IX/IY plus/minus d parameter
parseComponent (Parens_Var_ixyPlusD xyPos dPos) =
  makeParseParensParser $
  \cs -> parseSuccessFromCs cs
    >>= use (parseKeywordMap [("ix", False), ("iy", True)])
    >>= use (parseKeywordMap [("+", Ast_Plus), ("-", Ast_Minus)])
    >>= use (parseExpr)
    >>= used3 (\(isIy, plusMinus, expr) ->
          [Ast_PlusMinusD plusMinus dPos expr] ++
          if isIy
            then [Ast_Const $ shiftL (1 :: Word32) (32 - 1 - xyPos)]
            else []
        )

-- An '(n)' parameter (absolute addressing)
parseComponent (Parens_Var_n  start) = makeParseParensParser $ parseComponent (Var_n  start)

-- An '(nn)' parameter (absolute addressing)
parseComponent (Parens_Var_nn start) = makeParseParensParser $ parseComponent (Var_nn start)


-- | Make a parser for the target of another parser, wrapped in parentheses.
--   This really belongs with the meta parsers, but putting it there would
--   introduce a circular dependency. And this isn't used elsewhere.
makeParseParensParser :: Parser a -> Parser a
makeParseParensParser parser =
  \cs -> parseSuccessFromCs cs
    >>= ___ (parseOpenParens)
    >>= ___ (parseOptionalSpace)
    >>= use (parser)
    >>= ___ (parseOptionalSpace)
    >>= ___ (parseCloseParens)
    >>= used1 id


-- | A handy tool for constructing tables for register lookups
mapTable
  :: Int        -- bit position for destination start (MSB = 0)
  -> Int        -- bit length of each table entry
  -> [( String  -- text to parse
      , Word32  -- bits corresponding to text
     )] ->
     [( String  -- parsed text (duplicated)
      , [Ast]   -- resulting AST
     )]
mapTable start len table =
  map
    (\(str, val) -> (str, [Ast_Const $ shiftL val (32 - len - start)]))
    table


preprocessedInstructionSet :: [InstructionDef]
preprocessedInstructionSet = sortBy sorter instructionSet

-- | Reorder the instruction definitions to put ones that use absolute
--   expressions at the start.
-- 
--   When we see an instruction like `ld a,($1000)`, we must first try parse it
--   as an absolute instruction, not an immediate one where the parentheses
--   belong to the numeric expression.
--   (But if the instruction doesn't offer an absolute mode, the parentheses
--   will be considered part of the expression. Beware!
sorter :: InstructionDef -> InstructionDef -> Ordering
sorter a b | componentsHaveParens a = LT
           | componentsHaveParens b = GT
  where
    componentsHaveParens :: InstructionDef -> Bool
    componentsHaveParens (_, cs) = any componentHasParens cs

    componentHasParens :: Component -> Bool
    componentHasParens (Parens_Var_n _)  = True
    componentHasParens (Parens_Var_nn _) = True
    componentHasParens _                 = False
    -- There's no need to worry about the other Parens_ cases: I don't think
    -- they can cause ambiguity

sorter _ _ = EQ
