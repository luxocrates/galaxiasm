module Asm.Types.Pass1
  ( LineArtifact (LabelDefinition, Instruction, SymbolAssignment)
  , Pass1Artifact
  , Pass1Artifacts
  , PositionedString
  )
where

import Asm.Types.CharStream (CharStream)
import Asm.Types.LineAndSource (LineAndSource)


-- | Assembler artifacts generated by the end of pass1, if successful
type Pass1Artifacts = [Pass1Artifact]

type Pass1Artifact = (LineArtifact, LineAndSource)

-- | An isolated component of a line from an assembler source
data LineArtifact =
    LabelDefinition
      PositionedString  -- label name, with position tracing
  | Instruction
      CharStream        -- a CharStream, limited to trimmed instruction text
  | SymbolAssignment
      PositionedString  -- symbol name, with position tracing
      Int               -- value
  deriving (Show, Eq)

type PositionedString =
  ( String        -- label text
  , CharStream    -- first char of string
  , CharStream    -- first char after string
  )
