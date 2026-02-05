module Asm.Parsers.Directive (parseDirective) where

import Data.Bits (shiftL)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import Asm.ArtifactAccumulator (___, use, used0, used1, used2)
import Asm.BaseParser (parseSuccessFromCs)
import Asm.Parsers.Expr (parseExpr)
import Asm.Parsers.Meta (parseAny, parseKeyword, parseMaybe, peekWhile)
import Asm.Parsers.SelfContainedExpr (parseSelfContainedExpr)
import Asm.Parsers.Tokens
  ( parseDoubleQuotes
  , parseComma
  , parseEndOfLine
  , parseMandatorySpace
  , parseOptionalSpace
  , parseε
  )
import Asm.Types.AsmFailable
  ( AsmErrorDetails (AlignTooBigError, FillByteError, OrgTooBigError, OrgTooSmallError)
  , AsmErrorLoc (CsRange, Nowhere)
  , AsmFailable
  )
import Asm.Types.Expr (Expr)
import Asm.Types.Parser (ParseAttempt (ParseError, ParseSuccess), Parser)
import Asm.Types.Pass2
  ( Ast (Ast_Const, Ast_N, Ast_Nn)
  , OpcodeOrDirective
  , UnresolvedBits
  )


-- | Parse any assembler directive
parseDirective :: Parser OpcodeOrDirective
parseDirective = parseAny
  [ parseDotDb
  , parseDotDw
  , parseDotOrg
  , parseDotAlign
  , parseDotGString
  , parseChecksumBalance
  ]


-- | Parse .db/.byte directive, however many args
parseDotDb :: Parser OpcodeOrDirective
parseDotDb =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseAny
              [ parseKeyword ".db" ()
              , parseKeyword ".byte" ()
              ])
    >>= ___ (parseMandatorySpace)
    >>= use (parseCommaSeparatedExprs)
    >>= ___ (parseEndOfLine)
    >>= used1
          (\exs -> const $
            Right ((map (\ex -> (0, 1, [Ast_N 0 ex])) exs), False)
            )

-- | Parse .word directive, however many args
parseDotDw :: Parser OpcodeOrDirective
parseDotDw =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseAny
              [ parseKeyword ".dw" ()
              , parseKeyword ".word" ()
              ])
    >>= ___ (parseMandatorySpace)
    >>= use (parseCommaSeparatedExprs)
    >>= ___ (parseEndOfLine)
    >>= used1
          (\exs -> const $
            Right ((map (\ex -> (0, 2, [Ast_Nn 0 ex])) exs), False)
            )

-- | Parse .org directive with optional second arg
parseDotOrg :: Parser OpcodeOrDirective
parseDotOrg =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseCommonOrgOrAlign ".org")
    >>= \(cs, ((), (org, fill)))
          -> ParseSuccess
               ( cs
               , \addr -> do
                   checkAddr addr org
                   Right (bytesAsUnresolvedBitsList (org - addr) fill, False)
               )
  where
    checkAddr
      :: Int  -- current address
      -> Int  -- address requested
      -> AsmFailable ()
    checkAddr addr org
      | org < addr   = Left (Nowhere, OrgTooSmallError org addr)
      | org > 0xffff = Left (Nowhere, OrgTooBigError org)
      | otherwise    = Right ()


-- | Parse .align directive with optional second arg
parseDotAlign :: Parser OpcodeOrDirective
parseDotAlign =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseCommonOrgOrAlign ".align")
    >>= used1
          (\(align, fill) -> \addr ->
            let
              length = (align - (addr `mod` align)) `mod` align
            in
              do
                checkMemBounds $ addr + length
                Right (bytesAsUnresolvedBitsList length fill, False)
          )
  where
    checkMemBounds
      :: Int  -- proposed new address
      -> AsmFailable ()
    checkMemBounds addr
      | addr > 0xffff = Left (Nowhere, AlignTooBigError addr)
      | otherwise     = Right ()


-- | Parse .gstring directive
parseDotGString :: Parser OpcodeOrDirective
parseDotGString =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseKeyword ".gstring" ())
    >>= ___ (parseMandatorySpace)
    >>= ___ (parseDoubleQuotes)
    >>= use (parseGStringChars)
    >>= ___ (parseDoubleQuotes)
    >>= ___ (parseEndOfLine)
    >>= used1 (
          \str -> const $
            Right ((map (toUnresolvedBits . charToByte) str), False)
          )
  where
    toUnresolvedBits :: Int -> UnresolvedBits
    toUnresolvedBits charCode =
      ( shiftL ((fromIntegral charCode) :: Word32) (32 - 8), 1, [])

    -- we only have to translate those characters the parser let through
    charToByte :: Char -> Int
    charToByte ' ' = 0x40
    charToByte '-' = 0x5b
    charToByte 'p' = 0xd0
    charToByte 't' = 0xd1
    charToByte 's' = 0xd2
    charToByte ':' = 0xd3
    charToByte x   = ord x


-- | Parse the characters within the quotes for a .gstring directive
parseGStringChars :: Parser String
parseGStringChars = \cs -> ParseSuccess $ peekWhile isGStringChar cs
  where
    isGStringChar :: Char -> Bool
    isGStringChar c | c >= 'A' && c <= 'Z' = True
                    | c >= '0' && c <= '9' = True
    isGStringChar ' ' = True
    isGStringChar '-' = True
    isGStringChar 'p' = True
    isGStringChar 't' = True
    isGStringChar 's' = True
    isGStringChar ':' = True
    isGStringChar _   = False


-- | Parse .checksumbalance directive
parseChecksumBalance :: Parser OpcodeOrDirective
parseChecksumBalance =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseKeyword ".checksumbalance" ())
    >>= ___ (parseEndOfLine)
    >>= used0 (\_ -> \_ -> Right ([
      -- Insert a placeholder byte of zero for the checksum balance byte, so as
      -- to not disturb the count when we come to measure it
      (0, 1, [])
      ], True))


-- | Parses ', (expr)', for an expression that can be immediately resolved
parseAdditionalInt :: Parser Int
parseAdditionalInt =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseOptionalSpace)
    >>= ___ (parseComma)
    >>= ___ (parseOptionalSpace)
    >>= use (parseSelfContainedExpr)
    >>= used1 id

-- | Parses at least one expression in a comma-separated list
parseCommaSeparatedExprs :: Parser [Expr]
parseCommaSeparatedExprs =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseExpr)
    >>= use (parseMaybeCommaExprs)
    >>= used2 (\(ex, exs) -> ex:exs)

-- | Parses zero or more ', (expr)' sequences
parseMaybeCommaExprs :: Parser [Expr]
parseMaybeCommaExprs = parseAny
  [ parseCommaExprMaybeMore
  , parseε []
  ]

-- | Parses one or more ', (expr)' sequences
parseCommaExprMaybeMore :: Parser [Expr]
parseCommaExprMaybeMore =
  \cs -> ParseSuccess (cs, ())
    >>= ___ (parseComma)
    >>= ___ (parseOptionalSpace)
    >>= use (parseExpr)
    >>= use (parseMaybeCommaExprs)
    >>= used2 (\(expr, more) -> expr:more)

-- | Parses the structures common to .org and .align directives
parseCommonOrgOrAlign
  :: String  -- keyword for directive (inc. dot)
  -> Parser
     ( Int   -- mandatory first directive argument
     , Int   -- second directive argument, or 0
     )
parseCommonOrgOrAlign keyword =
  \cs -> (parseSuccessFromCs cs)
    >>= ___ (parseKeyword keyword ())
    >>= ___ (parseMandatorySpace)
    >>= use (parseSelfContainedExpr)
    >>= use (parseMaybe parseAdditionalInt)
    >>= ___ (parseEndOfLine)
    >>= \(cs', (((), param1), maybeFill)) ->
          let fill = fromMaybe 0 maybeFill in
          if (fill < (-128)) || (fill > (255))
          -- TODO: we're using the cs range for the entire directive here, but
          -- we'd ideally show the range just of the fill byte
          then ParseError (CsRange cs cs', FillByteError fill)
          else ParseSuccess (cs', (param1, fill))


-- | Make an UnresolvedBits list from a sequence of repeated bytes
bytesAsUnresolvedBitsList :: Int -> Int -> [UnresolvedBits]
bytesAsUnresolvedBitsList length fill =
  take length $ repeat (0, 1, [Ast_Const (shiftL (fromIntegral fill) 24)])
