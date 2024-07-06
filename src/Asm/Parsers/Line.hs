module Asm.Parsers.Line (parseLine) where

-- Note: if we try to force a compile fail by doing something like
--
--   some_value = !!!!
--
-- We might hope that that will choke the symbol parser, but in fact the symbol
-- parser will just fail, and it'll get interpreted as an instruction by
-- default. That would lead to some misleading error messages if we don't
-- somehow catch this case.
--

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Asm.ArtifactAccumulator (___, use, used1, used2)
import Asm.BaseParser (parseSuccessFromCs)
import Asm.CharStream (confine, takeChar)
import Asm.Parsers.Meta (parseAny, parseMaybe, peekWhile)
import Asm.Parsers.SelfContainedExpr (parseSelfContainedExpr)
import Asm.Parsers.Tokens
  ( parseColon
  , parseEndOfLine
  , parseEquals
  , parseLabel
  , parseOptionalSpace
  , parseSemicolon
  )
import Asm.Types.CharStream (CharStream)
import Asm.Types.Parser (ParseAttempt (ParseNearly, ParseSuccess), Parser)
import Asm.Types.Pass1
  ( LineArtifact (Instruction, LabelDefinition, SymbolAssignment)
  , PositionedString
  )


parseLine :: Parser [LineArtifact]
parseLine = parseAny [

  -- "  symbol = number ; comment"
  \cs -> (parseSuccessFromCs cs)
  >>= ___ (parseOptionalSpace)
  >>= use (parsePositionedLabel)
  >>= ___ (parseOptionalSpace)
  >>= ___ (parseEquals)
  >>= ___ (parseOptionalSpace)
  >>= use (parseSelfContainedExpr)
  >>= ___ (parseOptionalSpace)
  >>= ___ (parseMaybe parseComment)
  >>= ___ (parseEndOfLine)
  >>= used2
      (\(nameAndPos, value) -> [
        SymbolAssignment nameAndPos value
        ])
  ,

  -- "label: instruction ; comment"
  \cs -> (parseSuccessFromCs cs)
  >>= ___ (parseOptionalSpace)
  >>= use (parseMaybe parseLabelThenColon)
  >>= ___ (parseOptionalSpace)
  >>= use (parseMaybe parseStatementText)
  >>= ___ (parseOptionalSpace)
  >>= ___ (parseMaybe parseComment)
  >>= ___ (parseEndOfLine)
  >>= used2
      (\(maybeLabel, maybeInstruction) ->
        let
          maybeLabelList = case maybeLabel of
            Just nameAndPos -> [LabelDefinition nameAndPos]
            _               -> []
          maybeInstructionList = case maybeInstruction of
            Just cs'       -> [Instruction cs']
            _              -> []
        in
          maybeLabelList ++ maybeInstructionList
        )
  ]


parsePositionedLabel :: Parser PositionedString
parsePositionedLabel =
  \cs -> (parseSuccessFromCs cs)
    >>= use (parseLabel)
    >>= (\(cs', ((), label)) -> ParseSuccess (cs', (label, cs, cs')))


parseLabelThenColon :: Parser PositionedString
parseLabelThenColon =
  \cs -> (parseSuccessFromCs cs)
  >>= use (parsePositionedLabel)
  >>= ___ (parseColon)
  >>= used1 id


-- Succeeds only if it gets something
--
parseStatementText :: Parser
   CharStream  -- A clone of the input CharStream, confined to the
               -- whitespace-trimmed statement

parseStatementText cs =
  let
    (cs', text) = peekWhile isNotSemicolon cs
    trimmedText = trim text
  in
    if trimmedText == ""
      then ParseNearly cs
      else ParseSuccess (cs', confine cs (length trimmedText))
  where
    isNotSemicolon = (/= ';')
    trim = dropWhileEnd isSpace


parseAnythingUntilEnd :: Parser ()
parseAnythingUntilEnd cs =
  case takeChar cs of
    (cs', Nothing) -> ParseSuccess (cs', ())
    (cs', Just _)  -> parseAnythingUntilEnd cs'


parseComment :: Parser ()
parseComment =
  \cs -> (parseSuccessFromCs cs)
  >>= ___ (parseSemicolon)
  >>= ___ (parseAnythingUntilEnd)

