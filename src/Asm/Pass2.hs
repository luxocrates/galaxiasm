module Asm.Pass2 (pass2, printPass2Result) where

import Control.Monad (foldM)
import Data.Map (empty, insert, member, toList)
import Data.Maybe (isJust)

import Asm.Parsers.Directive (parseDirective)
import Asm.Parsers.Meta (parseAny)
import Asm.Parsers.Opcode (parseOpcode)
import Asm.ShowError (showError)
import Asm.Types.AsmFailable
  ( AsmErrorDetails
    ( SyntaxError
    , AddressError
    , CsBalanceRedefError
    , LocalDefButNoParentError
    , RedefLabelError
    , RedefSymbolError
    )
  , AsmErrorLoc (CsPos, CsRange)
  , AsmFailable
  )
import Asm.Types.LineAndSource (LineAndSource)
import Asm.Types.Parser (ParseAttempt (ParseError, ParseNearly, ParseSuccess))
import Asm.Types.Pass1
  ( LineArtifact (Instruction, LabelDefinition, SymbolAssignment)
  , Pass1Artifact
  , Pass1Artifacts
  , PositionedString
  )
import Asm.Types.Pass2
  ( Pass2Artifacts
  , SymTable
  , SymbolType (Equ, Label)
  , UnresolvedBitsInContext
  )


-- | Pass 2: take in the line artifacts from pass 1; build an AST per-artifact
--   for instructions, and create a map for symbols and labels
pass2 :: Pass1Artifacts -> AsmFailable Pass2Artifacts
pass2 pass1Artifacts =
  do
    acc <- foldM folder startingPass2Acc pass1Artifacts
    return
      ( accSymbols acc
      , accUnresolved acc
      , accCsBalancePos acc
      )
  where
    startingPass2Acc =
      Pass2Acc
        { accSymbols        = Data.Map.empty
        , accParentLabel    = Nothing
        , accUnresolved     = []
        , accPc             = 0
        , accCsBalancePos   = Nothing
        }


data Pass2Acc =
  Pass2Acc
  { accSymbols      :: SymTable
  , accParentLabel  :: Maybe String
  , accUnresolved   :: [UnresolvedBitsInContext]
  , accPc           :: Int
  , accCsBalancePos :: Maybe Int
  }


-- | Update the fold accumulator with a single line-artifact
folder :: Pass2Acc -> Pass1Artifact -> AsmFailable Pass2Acc
-- Action a label declaration
folder acc (LabelDefinition nameAndPos, source) =
  addLabel acc nameAndPos (accPc acc) source

-- Action a symbol assignment
folder acc (SymbolAssignment name value, source) =
  addSymbol acc name value source

-- Action an instruction
folder acc (Instruction cs, numberAndLine) =
  case parseAny [parseOpcode, parseDirective] cs of
    ParseSuccess (cs', opcodeOrDirective) -> do

      -- Now that the destination address is known, pass it to the function
      -- returned by the opcode/directive parser to get the actual unresolved
      -- bit chunks
      (chunks, isChecksumBalance) <- opcodeOrDirective (accPc acc)
      (
        let
          len = foldl (\acc (_, len, _) -> acc + len) 0 chunks
          newPc = (accPc acc) + len
        in
          -- If we've just written a byte to $ffff, the newPc will be $10000,
          -- and that's OK if this is the last instruction or data. But if newPc
          -- is more than that, we must have blown past the 16-bit address
          -- space, so raise an error.
          if newPc > 0x10000
          then
            Left
              ( CsRange cs cs'
              , AddressError (
                  -- offending byte was one of the last ones laid down, not the
                  -- next one to come
                  newPc - 1
                )
              )
          -- Bark if we're trying to specify the .checksumbalance pos a 2nd time
          else if isRedef (accCsBalancePos acc) isChecksumBalance
            then Left (CsRange cs cs', CsBalanceRedefError)

          -- Add the unresolved bit chunks to the accumulator, and advance the
          -- PC to just after them
          else Right $
            acc
              { accPc           = newPc
              , accUnresolved   = accUnresolved acc
                                  ++ map
                                    (\unresolvedBits ->
                                      ( accPc acc
                                      , accParentLabel acc
                                      , unresolvedBits
                                      , numberAndLine
                                      )
                                    )
                                    chunks
              , accCsBalancePos = if isChecksumBalance
                                    then Just $ accPc acc
                                    else Nothing
              }
        )
    ParseNearly cs'  -> Left (CsPos cs', SyntaxError)
    ParseError err   -> Left err
  where
    isRedef :: Maybe Int -> Bool -> Bool
    isRedef existing isChecksumBalance = isChecksumBalance && isJust existing


-- Add a label definition to the fold accumulator
addLabel
  :: Pass2Acc
  -> PositionedString
  -> Int
  -> LineAndSource
  -> AsmFailable Pass2Acc
addLabel acc (name, start, end) value source =
  do
    -- Expand the label if it's a local label (begins with underscore)
    fullName <-
      if isLocalLabel then
        case accParentLabel acc of
          Just str -> Right $ str ++ name
          Nothing  -> Left (CsRange start end, LocalDefButNoParentError)
      else return name

    -- Add the label to the symbol table, and update accParentLabel if this
    -- isn't a local label
    case Data.Map.member fullName symbols of
      True  -> Left (CsRange start end, RedefLabelError fullName)
      False -> Right
        acc
          { accSymbols = Data.Map.insert
                           fullName
                           (value, (Label, source))
                           symbols
          , accParentLabel = if isLocalLabel
                               then accParentLabel acc
                               else Just name
          }
  where
    symbols = accSymbols acc
    isLocalLabel = (name !! 0) == '_'


-- Add a symbol definition to the fold accumulator
addSymbol
  :: Pass2Acc
  -> PositionedString
  -> Int
  -> LineAndSource
  -> AsmFailable Pass2Acc
addSymbol acc (name, start, end) value source =
  case Data.Map.member name symbols of
    True  -> Left (CsRange start end, RedefSymbolError name)
    False -> Right
      acc
        { accSymbols = Data.Map.insert name (value, (Equ, source)) symbols }
  where symbols = accSymbols acc


showDict :: SymTable -> String
showDict d
  = "{\n"
  ++ entries
  ++ "}"
  where
    entries = foldl folder "" (toList d)
    folder acc (k, (v, _)) = acc ++ "  " ++ k ++ ": " ++ (show v) ++ "\n"


printPass2Artifacts :: Pass2Artifacts -> String
printPass2Artifacts (symbols, thunks, csBalancePos)
  =  "Symbols: " ++ (showDict symbols) ++ "\n"
  ++ "Thunks: " ++ show (length thunks) ++ "\n"
  ++ "CsBalancePos: " ++ show (csBalancePos) ++ "\n"

printPass2Result :: AsmFailable Pass2Artifacts -> String
printPass2Result (Left err)   = showError err
printPass2Result (Right arts) = printPass2Artifacts arts
