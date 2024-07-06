module Asm.ShowError (showError) where

import Common.String (showHex4)

import Asm.Types.AsmFailable (AsmError, AsmErrorDetails (..), AsmErrorLoc (..))
import Asm.Types.CharStream (CharStream (CS))


-- | Reifies an AsmError to a description and an excerpt of the source,
--   underlining the error location
showError :: AsmError -> String
showError (loc, details) = "Error: " ++ showDetails details
                           ++ "\n" ++ showLoc loc ++ "\n"

showLoc :: AsmErrorLoc -> String
showLoc Nowhere          = ""
showLoc (CsPos cs)       = showCharStreams cs cs
showLoc (CsRange cs cs') = showCharStreams cs cs'

{-| Pretty-prints a CharStream pair, in a format suitable for an error
    display, highlighting the range between the two
-}
--   eg.
--
--   in source.s:
--        |
--     27 | ld $fffff, a
--        |    ^^^^^^
--
showCharStreams
  :: CharStream  -- CharStream with cursor at start point for underline
  -> CharStream  -- CharStream with cursor at last point for underline
  -> String
showCharStreams
  (CS text cursor1 _ (filename, lineNum))
  (CS _    cursor2 _ _                  )
  = "in " ++ filename ++ ":\n"
    ++ emptyPre ++ "\n"
    ++ lineNumPre ++ text ++ "\n"
    ++ emptyPre
      ++ replicate cursor1 ' '
      ++ replicate (max 1 (cursor2 - cursor1)) '^'
      ++ "\n"
  where
    indent     = "  "
    lnStr      = show lineNum
    lnStrLen   = length lnStr
    emptyPre   = indent ++ replicate lnStrLen ' ' ++ " | "
    lineNumPre = indent ++ lnStr ++ " | "


-- | Reifies AsmErrorDetails to a string
showDetails :: AsmErrorDetails -> String
showDetails (OtherError str)            = str
showDetails LocalUseButNoParentError    = "attempting to use a local label, "
                                          ++ "but there's no parent"
showDetails LocalDefButNoParentError    = "attempting to define a local label, "
                                          ++ "but there's no parent label"
showDetails (RedefLabelError label)     = label ++ " has already been defined"
showDetails (RedefSymbolError symbol)   = symbol ++ " has already been defined"
showDetails (UndefSymbolError symbol)   = "no such symbol: '" ++ symbol ++ "'"
showDetails UnavailSymbolError          = "symbols can't be used in this definition"
showDetails InvalidRstError             = "RST parameter can be $0, $8, $10, $18, $20, $28, $30 or $38"
showDetails (BoundsError val min max)   = "value " ++ show val
                                          ++ " is out of range. It must be between "
                                          ++ show min ++ " and " ++ show max
                                          ++ "."
showDetails (OrgTooSmallError org addr) = ".org destination (" ++ showHex4 org
                                          ++ ") is before current memory address ("
                                          ++ showHex4 addr ++ ")"
showDetails (OrgTooBigError _)          = ".org would go beyond addressable memory"
showDetails (AlignTooBigError addr)     = ".align would go beyond end of memory ("
                                          ++ showHex4 addr ++ ")"
showDetails (FillByteError _)           = "Fill byte must be between -128 and 255"
showDetails SyntaxError                 = "Parse error"
showDetails (AddressError addr)         = "Target address (" ++ showHex4 addr
                                          ++ ") went out of range"
showDetails CsBalanceRedefError         = ".checksumbalance re-defined"
