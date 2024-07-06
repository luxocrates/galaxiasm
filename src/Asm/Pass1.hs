module Asm.Pass1 (pass1) where

import Control.Monad (foldM)

import Asm.CharStream (makeCs)
import Asm.Parsers.Line (parseLine)
import Asm.Types.AsmFailable
  ( AsmErrorDetails (SyntaxError)
  , AsmErrorLoc (CsPos)
  , AsmFailable
  )
import Asm.Types.Parser (ParseAttempt (ParseError, ParseNearly, ParseSuccess))
import Asm.Types.Pass1 ( LineAndSource, Pass1Artifact, Pass1Artifacts)


-- | Pass 1: take in a whole file's worth of text, and break down each line into
--   a list of things the line is trying to do (intructions are left as text,
--   not parsed), augmented with trace information
pass1
  :: String  -- text to assemble
  -> String  -- filename to attribute to text
  -> AsmFailable Pass1Artifacts
pass1 program filename = foldM acc1Line [] (breakIntoLines program filename)


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


-- | Break a corpus into lines, each augmented with source instrumentation
breakIntoLines
  :: String  -- text
  -> String  -- filename
  -> [LineAndSource]
breakIntoLines program filename =
  zip
    (lines program)
    (map (\lineNum -> (filename, lineNum)) [1..])
