module Disasm.Disassemble (disassemble) where

import Data.Bits (shift, shiftL, shiftR, (.&.), (.|.))
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Data.Word (Word32, Word8)

import Common.BitOps (byteSwap)
import Common.String (padEnd, showHex2, showHex4)
import InstructionSet (Component (..), InstructionDef, instructionSet)


-- | A pure disassembler: Z80 machine code in, text out
disassemble :: [Word8] -> String
disassemble w8s =
  -- The disassembler works by looking at the next four bytes of the binary
  -- stream and pattern-matching them as a 32-bit word (since no instructions
  -- are longer than four bytes).
  -- 
  -- To ensure that it always has enough bytes to do that, we pad the end with
  -- three extra bytes, but don't count them among the notional stream length.
  helper (w8s ++ [0, 0, 0]) (length w8s) 0
  where
    helper
      :: [Word8]  -- bytes to disassemble (padded at end)
      -> Int      -- pre-padding number of bytes
      -> Int      -- program counter
      -> String
    helper _ 0 _  = "" -- no more input bytes remain: we're done
    helper w8s len pc
      = let
          (str, adv) = disasmOne w8s pc
        in
          showHex4 pc ++ ": " ++
          str ++ "\n" ++ helper (drop adv w8s) (len - adv) (pc + adv)


-- | Disassemble a single instruction from a byte stream
disasmOne
  :: [Word8]    -- bytes to disassemble
  -> Int        -- address
  -> ( String   -- disassembled instruction
     , Int      -- number of bytes to advance
     )
disasmOne w8s pc =
  let w32 = (shift (fromIntegral ((w8s) !! 0) :: Word32) 24)
        .|. (shift (fromIntegral ((w8s) !! 1) :: Word32) 16)
        .|. (shift (fromIntegral ((w8s) !! 2) :: Word32)  8)
        .|. (shift (fromIntegral ((w8s) !! 3) :: Word32)  0)
  in
    disasmOne_W32 w32 (length w8s - 3) pc quickMatchInstructions


-- | Disassemble a left-aligned instruction in a 32-bit word, by recursively
--   pattern-matching against a list of candidates
disasmOne_W32
  :: Word32                   -- left-aligned bytes to disassemble
  -> Int                      -- number of bytes available for disassembly
  -> Int                      -- address of head instruction
  -> [QuickMatchInstruction]  -- candidate instructions
  -> ( String                 -- disassembly
     , Int                    -- number of bytes to advance
     )

-- If nothing's matched, just call it data
disasmOne_W32 w32 _ _ [] = (".db " ++ showHex2 (maskAndMove 8 0 w32), 1)

disasmOne_W32 w32 avail pc ((must, mustNot, prodBytes, prod) : ls)
  -- If the instruction is longer than the number of unprocessed bytes
  -- remaining, it's not a match
  | prodBytes > avail
    = next

  -- Compare the w32 bits to the pattern: if it has ones in all the 'must'
  -- places, and zeros in all the 'mustNot' places, then consider its candidacy
  -- further
  | (w32 .&. must == must) && (w32 .&. mustNot == 0)
    = let compMap = map (componentMatch w32 pc) prod in
      -- Test if all of the components of the instruction can match. In spite of
      -- having passed the 'must' and 'mustNot' tests to get this far, the match
      -- may yet fail. For example, if the candidate instruction has an 'r'
      -- parameter, but the bits in that slot are the invalid combination '110'.
      if all isJust compMap
        then
          let
            stringMap = map fromJust compMap
            opcode = head stringMap
          in
            ( opcode ++
              if length stringMap == 1
                then
                  -- Opcode with no parameters
                  ""
                else
                  replicate (5 - length opcode) ' ' ++
                  intercalate "," (drop 1 stringMap)
            , prodBytes
            )
      else
        next
  | otherwise
    = next
  where
    -- Candidate didn't match. Recurse to try the next one.
    next = disasmOne_W32 w32 avail pc ls


-- | Extract a number of bits from a Word32, and right-align them.
--   For example, maskAndMove 4 3 would be a function that extracts
--   xxxABCDxxxxxxxxxxxxxxxxxxxxxxxxx to 0000000000000000000000000000ABCD
maskAndMove
  :: Int     -- length of sequence to extract
  -> Int     -- start of sequence within word (0 = MSB)
  -> Word32  -- word containing bit sequence
  -> Word32
maskAndMove length start w32
  = (shiftR w32 (32 - start - length)) .&. ((shiftL 1 length) - 1)


-- | Generate a disassembly string for a component if it's a valid bit sequence
--   within a host word
componentMatch
  :: Word32        -- left-aligned head bytes of stream
  -> Int           -- address of start of instruction (for relative addressing)
  -> Component     -- production component
  -> Maybe String
componentMatch _ _ (K str)        = Just str
componentMatch _ _ (Parens_K str) = Just $ "(" ++ str ++ ")"

componentMatch w32 _ (Var_b start)
  = Just $ show $ maskAndMove 3 start w32

componentMatch w32 _ (Var_n start)
    = Just $ showHex2 (maskAndMove 8 start w32)

componentMatch w32 _ (Var_p start)
  = case maskAndMove 3 start w32 of
      0b000 -> Just "$00"
      0b001 -> Just "$08"
      0b010 -> Just "$10"
      0b011 -> Just "$18"
      0b100 -> Just "$20"
      0b101 -> Just "$28"
      0b110 -> Just "$30"
      0b111 -> Just "$38"
      _     -> error "should be unreachable"

componentMatch w32 _ (Var_r start)
  = case maskAndMove 3 start w32 of
      0b111 -> Just "a"
      0b000 -> Just "b"
      0b001 -> Just "c"
      0b010 -> Just "d"
      0b011 -> Just "e"
      0b100 -> Just "h"
      0b101 -> Just "l"
      _     -> Nothing

componentMatch w32 _ (Var_ixy start)
  = Just $ "i" ++ if maskAndMove 1 start w32 == 0 then "x" else "y"

componentMatch w32 _ (Var_cc start)
  = case maskAndMove 3 start w32 of
      0b000 -> Just "nz"
      0b001 -> Just "z"
      0b010 -> Just "nc"
      0b011 -> Just "c"
      0b100 -> Just "po"
      0b101 -> Just "pe"
      0b110 -> Just "p"
      0b111 -> Just "m"
      _     -> error "should be unreachable"

componentMatch w32 _ (Var_dd start)
  = case maskAndMove 2 start w32 of
      0b00 -> Just "bc"
      0b01 -> Just "de"
      0b10 -> Just "hl"
      0b11 -> Just "sp"
      _    -> error "should be unreachable"

componentMatch w32 _ (Var_nn start)
  = Just $ showHex4 (byteSwap $ maskAndMove 16 start w32)

componentMatch w32 _ (Var_pp start)
  = case maskAndMove 2 start w32 of
      0b00 -> Just "bc"
      0b01 -> Just "de"
      0b10 -> Just "ix"
      0b11 -> Just "sp"
      _    -> error "should be unreachable"

componentMatch w32 _ (Var_qq start)
  = case maskAndMove 2 start w32 of
      0b00 -> Just "bc"
      0b01 -> Just "de"
      0b10 -> Just "hl"
      0b11 -> Just "af"
      _    -> error "should be unreachable"

componentMatch w32 _ (Var_rr start)
  = case maskAndMove 2 start w32 of
      0b00 -> Just "bc"
      0b01 -> Just "de"
      0b10 -> Just "iy"
      0b11 -> Just "sp"
      _    -> error "should be unreachable"

componentMatch w32 pc (Var_ss start) = componentMatch w32 pc (Var_dd start)

componentMatch w32 pc (Var_eMinus2 start)
  = Just $ showHex4 ((maskAndMove 8 start w32) + 2 + (fromIntegral pc))

componentMatch w32 pc (Parens_Var_ixy start)
  -- Parens_Var_ixy is silly. It's used only in 'jp (ix)'/'jp (iy)', which
  -- should really have been called 'jp ix'/'jp iy'.
  = Just $ "(" ++ (fromJust $ componentMatch w32 pc (Var_ixy start)) ++ ")"

componentMatch w32 _ (Parens_Var_ixyPlusD xyStart dStart)
  = let
      xyChar = if maskAndMove 1 xyStart w32 == 0 then "x" else "y"
      dByte  = maskAndMove 8 dStart w32
      dStr   = if dByte > 0x7f
                 then "-" ++ showHex2 (256 - dByte)
                 else "+" ++ showHex2 dByte
    in
      Just $ "(i" ++ xyChar ++ dStr ++ ")"

componentMatch w32 _ (Parens_Var_n start)
  = Just $ "(" ++ (showHex2 $ maskAndMove 8 start w32) ++ ")"

componentMatch w32 _ (Parens_Var_nn start)
  = Just $ "(" ++ (showHex4 (byteSwap $ maskAndMove 16 start w32)) ++ ")"


-- | An instruction definition rearranged to allow for quick pattern matching
type QuickMatchInstruction =
  ( Word32        -- 'musts': bits that must be set for this instruction
  , Word32        -- 'must-nots': bits that must not be set for this instruction
  , Int           -- length of instruction, in bytes
  , [Component]   -- breakdown of components
  )

-- | To test the input stream against the instruction set string patterns, one
--   character at a time, would be very slow. This preprocessor converts the
--   patterns into a tuple that contains bit masks that we can use to ascertain
--   if we have an instruction match; the length of the instruction, and the
--   constituent components
quickMatchInstructions :: [QuickMatchInstruction]
quickMatchInstructions = map xform instructionSet
  where
    xform :: InstructionDef -> QuickMatchInstruction
    xform (pattern, components) =
      let
        in32Chars  = padEnd 32 '-' pattern
        mustStr    = map (\x -> if x == '-' then '0' else x  ) in32Chars
        mustNotStr = map (\x -> if x == '0' then '1' else '0') in32Chars
      in
        ( strToW32 mustStr
        , strToW32 mustNotStr
        , div (length pattern) 8
        , components
        )


-- | Converts a string of 32 '1's or '0's to a Word32
strToW32 :: String -> Word32
strToW32 x = (fromIntegral $ binCharsToInt x) :: Word32
  where
    binCharsToInt :: String -> Int
    binCharsToInt [] = 0
    binCharsToInt cs = (if last cs == '1' then 1 else 0) + (2 * binCharsToInt (init cs))
