module Asm.Parsers.Tokens
  ( parseDollar
  , parseDoubleQuotes
  , parseLessThan
  , parseGreaterThan
  , parseDigits
  , parseHexDigits
  , parseEquals
  , parseColon
  , parseSemicolon
  , parseComma
  , parsePlus
  , parseMinus
  , parseAmpersand
  , parseBar
  , parseAsterisk
  , parseOpenParens
  , parseCloseParens
  , parseOptionalSpace
  , parseMandatorySpace
  , parseEndOfLine
  , parseLabel
  , parseFilename
  , parseε
  )
where

import Data.Char (isAlpha, isDigit, isHexDigit)

import Asm.CharStream (takeChar)
import Asm.Parsers.Meta (parseKeyword, peekWhile)
import Asm.Types.Parser (ParseAttempt (ParseNearly, ParseSuccess), Parser)


parseDollar       :: Parser ()
parseDoubleQuotes :: Parser ()
parseLessThan     :: Parser ()
parseGreaterThan  :: Parser ()
parseEquals       :: Parser ()
parseColon        :: Parser ()
parseSemicolon    :: Parser ()
parseComma        :: Parser ()
parsePlus         :: Parser ()
parseMinus        :: Parser ()
parseAmpersand    :: Parser ()
parseBar          :: Parser ()
parseAsterisk     :: Parser ()
parseOpenParens   :: Parser ()
parseCloseParens  :: Parser ()

parseDollar       = parseKeyword "$"  ()
parseDoubleQuotes = parseKeyword "\"" ()
parseLessThan     = parseKeyword "<"  ()
parseGreaterThan  = parseKeyword ">"  ()
parseEquals       = parseKeyword "=" ()
parseColon        = parseKeyword ":" ()
parseSemicolon    = parseKeyword ";" ()
parseComma        = parseKeyword "," ()
parsePlus         = parseKeyword "+" ()
parseMinus        = parseKeyword "-" ()
parseAmpersand    = parseKeyword "&" ()
parseBar          = parseKeyword "|" ()
parseAsterisk     = parseKeyword "*" ()
parseOpenParens   = parseKeyword "(" ()
parseCloseParens  = parseKeyword ")" ()


parseDigits    :: Parser String
parseHexDigits :: Parser String

parseDigits    = parseAtLeastOne isDigit
parseHexDigits = parseAtLeastOne isHexDigit


-- | Parses one vetted character or more
parseAtLeastOne :: (Char -> Bool) -> Parser String
parseAtLeastOne test cs =
  let (cs', chars) = peekWhile test cs in
  if length chars > 0
    then ParseSuccess (cs', chars)
    else ParseNearly cs


parseOptionalSpace :: Parser ()
parseOptionalSpace cs =
  case takeChar cs of
    (cs', Just ' ') -> parseOptionalSpace cs'
    _               -> ParseSuccess (cs, ())

parseMandatorySpace :: Parser ()
parseMandatorySpace cs =
  case takeChar cs of
    (cs', Just ' ') -> parseOptionalSpace cs'
    _               -> ParseNearly cs


-- | Asserts there's no more characters to parse
parseEndOfLine :: Parser ()
parseEndOfLine cs =
  case takeChar cs of
    (cs', Nothing) -> ParseSuccess (cs', ())
    _              -> ParseNearly cs


-- | Parses a string that would be a valid label or symbol. Valid symbols can
--   include letters, numbers and _, but must not start with a number.
--   Symbols cannot use the name of a register.
parseLabel :: Parser String
parseLabel cs =
  case peekWhile isLabelChar cs of
    (_,   ""   )                        -> ParseNearly cs
    (_,  "a"   )                        -> ParseNearly cs
    (_,  "b"   )                        -> ParseNearly cs
    (_,  "c"   )                        -> ParseNearly cs
    (_,  "d"   )                        -> ParseNearly cs
    (_,  "e"   )                        -> ParseNearly cs
    (_,  "h"   )                        -> ParseNearly cs
    (_,  "l"   )                        -> ParseNearly cs
    (_,  "bc"  )                        -> ParseNearly cs
    (_,  "de"  )                        -> ParseNearly cs
    (_,  "hl"  )                        -> ParseNearly cs
    (_,  "sp"  )                        -> ParseNearly cs
    (_,  "ix"  )                        -> ParseNearly cs
    (_,  "iy"  )                        -> ParseNearly cs
    (_,  "i"   )                        -> ParseNearly cs
    (_,  "r"   )                        -> ParseNearly cs
    (cs', label) | isDigit (head label) -> ParseNearly cs
                 | otherwise            -> ParseSuccess (cs', label)
  where
    isLabelChar c = (c == '_') || isDigit c || isAlpha c


-- | Parses a string that would be a valid filename, defined as any string of
--   characters until an end quote. We're not sophisticated enough to allow
--   escape codes for an intended double quote.
parseFilename :: Parser String
parseFilename cs =
  let
    (cs', filename) = peekWhile isNotEndQuote cs
  in
    if filename == ""
      then ParseNearly cs
      else ParseSuccess (cs', filename)
  where
    isNotEndQuote = (/= '"')


-- | Always succeeds, without consuming more characters
parseε :: a -> Parser a
parseε x = \cs -> ParseSuccess (cs, x)
