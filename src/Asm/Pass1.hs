module Asm.Pass1 (pass1) where

import Control.Monad (foldM)

import Asm.CharStream (makeCs)
import Asm.Parsers.Line (parseLine)
import Asm.Types.AsmFailable
  ( AsmErrorDetails (SyntaxError)
  , AsmErrorLoc (CsPos)
  , AsmFailable
  )
import Asm.Types.LineAndSource (LineAndSource)
import Asm.Types.Parser (ParseAttempt (ParseError, ParseNearly, ParseSuccess))
import Asm.Types.Pass1 ( Pass1Artifact, Pass1Artifacts)


-- | Pass 1: Iterate through all the attributed source lines (.include's have
--   already been expanded), and break down each into a list of things the line
--   is trying to do (intructions are left as text, not parsed)
pass1
  :: [LineAndSource]  -- text to assemble
  -> AsmFailable Pass1Artifacts
pass1 attribLines = foldM acc1Line [] attribLines


-- | Parse a single line and absorb its artifacts into the accumulator, or fail
acc1Line :: [Pass1Artifact] -> LineAndSource -> AsmFailable [Pass1Artifact]
acc1Line acc lineAndSource =
  let (line, source) = lineAndSource in
  case parseLine (makeCs line source) of
    ParseNearly cs
      -> Left (CsPos cs, SyntaxError)
    ParseError err
      -> Left err
    ParseSuccess (_, arts)
      -> Right $ acc ++ map (, lineAndSource) arts
