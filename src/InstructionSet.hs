module InstructionSet (instructionSet, InstructionDef, Component (..)) where

-- | This table is at the nexus of the assembler and disassembler: it defines
--   the relationship between bit patterns in machine code and their syntactic
--   equivalents in assembly.
instructionSet :: [InstructionDef]
instructionSet =
  [ -- 8-bit load group (manual p70)
    ("01------",                          [K "ld", Var_r 2, Var_r 5])
  , ("00---110--------",                  [K "ld", Var_r 2, Var_n 8])
  , ("01---110",                          [K "ld", Var_r 2, Parens_K "hl"])
  , ("11-1110101---110--------",          [K "ld", Var_r 10, Parens_Var_ixyPlusD 2 16])
  , ("01110---",                          [K "ld", Parens_K "hl", Var_r 5])
  , ("11-1110101110-----------",          [K "ld", Parens_Var_ixyPlusD 2 16, Var_r 13])
  , ("00110110--------",                  [K "ld", Parens_K "hl", Var_n 8])
  , ("11-1110100110110----------------",  [K "ld", Parens_Var_ixyPlusD 2 16, Var_n 24])
  , ("00001010",                          [K "ld", K "a", Parens_K "bc"])
  , ("00011010",                          [K "ld", K "a", Parens_K "de"])
  , ("00111010----------------",          [K "ld", K "a", Parens_Var_nn 8])
  , ("00000010",                          [K "ld", Parens_K "bc", K "a"])
  , ("00010010",                          [K "ld", Parens_K "de", K "a"])
  , ("00110010----------------",          [K "ld", Parens_Var_nn 8, K "a"])
  , ("1110110101010111",                  [K "ld", K "a", K "i"])
  , ("1110110101011111",                  [K "ld", K "a", K "r"])
  , ("1110110101000111",                  [K "ld", K "i", K "a"])
  , ("1110110101001111",                  [K "ld", K "r", K "a"])

  -- 16-bit load group (manual p98)
  , ("00--0001----------------",          [K "ld", Var_dd 2, Var_nn 8])
  , ("11-1110100100001----------------",  [K "ld", Var_ixy 2, Var_nn 16])
  , ("00101010----------------",          [K "ld", K "hl", Parens_Var_nn 8])
  , ("1110110101--1011----------------",  [K "ld", Var_dd 10, Parens_Var_nn 16]) -- NOTE: if used with HL, would compile back to above
  , ("11-1110100101010----------------",  [K "ld", Var_ixy 2, Parens_Var_nn 16])
  , ("00100010----------------",          [K "ld", Parens_Var_nn 8, K "hl"])
  , ("1110110101--0011----------------",  [K "ld", Parens_Var_nn 16, Var_dd 10]) -- NOTE: if used with HL, would compile back to above
  , ("11-1110100100010----------------",  [K "ld", Parens_Var_nn 16, Var_ixy 2])
  , ("11111001",                          [K "ld", K "sp", K "hl"])
  , ("11-1110111111001",                  [K "ld", K "sp", Var_ixy 2])
  , ("11--0101",                          [K "push", Var_qq 2])
  , ("11-1110111100101",                  [K "push", Var_ixy 2])
  , ("11--0001",                          [K "pop", Var_qq 2])
  , ("11-1110111100001",                  [K "pop", Var_ixy 2])

  -- Exchange, block transfer, and search group (manual p123)
  , ("11101011",                          [K "ex", K "de", K "hl"])
  , ("00001000",                          [K "ex", K "af", K "af'"])
  , ("11011001",                          [K "exx"])
  , ("11100011",                          [K "ex", Parens_K "sp", K "hl"])
  , ("11-1110111100011",                  [K "ex", Parens_K "sp", Var_ixy 2])
  , ("1110110110100000",                  [K "ldi"])
  , ("1110110110110000",                  [K "ldir"])
  , ("1110110110101000",                  [K "ldd"])
  , ("1110110110111000",                  [K "lddr"])
  , ("1110110110100001",                  [K "cpi"])
  , ("1110110110110001",                  [K "cpir"])
  , ("1110110110101001",                  [K "cpd"])
  , ("1110110110111001",                  [K "cpdr"])

  -- 8-bit arithmetic group (manual p144)
  , ("10000---",                          [K "add", K "a", Var_r 5])
  , ("11000110--------",                  [K "add", K "a", Var_n 8])
  , ("10000110",                          [K "add", K "a", Parens_K "hl"])
  , ("11-1110110000110--------",          [K "add", K "a", Parens_Var_ixyPlusD 2 16])

  , ("10001---",                          [K "adc", K "a", Var_r 5])
  , ("11001110--------",                  [K "adc", K "a", Var_n 8])
  , ("10001110",                          [K "adc", K "a", Parens_K "hl"])
  , ("11-1110110001110--------",          [K "adc", K "a", Parens_Var_ixyPlusD 2 16])

  , ("10010---",                          [K "sub", Var_r 5])
  , ("11010110--------",                  [K "sub", Var_n 8])
  , ("10010110",                          [K "sub", Parens_K "hl"])
  , ("11-1110110010110--------",          [K "sub", Parens_Var_ixyPlusD 2 16])

  , ("10011---",                          [K "sbc", K "a", Var_r 5])
  , ("11011110--------",                  [K "sbc", K "a", Var_n 8])
  , ("10011110",                          [K "sbc", K "a", Parens_K "hl"])
  , ("11-1110110011110--------",          [K "sbc", K "a", Parens_Var_ixyPlusD 2 16])

  , ("10100---",                          [K "and", Var_r 5])
  , ("11100110--------",                  [K "and", Var_n 8])
  , ("10100110",                          [K "and", Parens_K "hl"])
  , ("11-1110110100110--------",          [K "and", Parens_Var_ixyPlusD 2 16])

  , ("10110---",                          [K "or", Var_r 5])
  , ("11110110--------",                  [K "or", Var_n 8])
  , ("10110110",                          [K "or", Parens_K "hl"])
  , ("11-1110110110110--------",          [K "or", Parens_Var_ixyPlusD 2 16])

  , ("10101---",                          [K "xor", Var_r 5])
  , ("11101110--------",                  [K "xor", Var_n 8])
  , ("10101110",                          [K "xor", Parens_K "hl"])
  , ("11-1110110101110--------",          [K "xor", Parens_Var_ixyPlusD 2 16])

  , ("10111---",                          [K "cp", Var_r 5])
  , ("11111110--------",                  [K "cp", Var_n 8])
  , ("10111110",                          [K "cp", Parens_K "hl"])
  , ("11-1110110111110--------",          [K "cp", Parens_Var_ixyPlusD 2 16])

  , ("00---100",                          [K "inc", Var_r 2])
  , ("00110100",                          [K "inc", Parens_K "hl"])
  , ("11-1110100110100--------",          [K "inc", Parens_Var_ixyPlusD 2 16])

  , ("00---101",                          [K "dec", Var_r 2])
  , ("00110101",                          [K "dec", Parens_K "hl"])
  , ("11-1110100110101--------",          [K "dec", Parens_Var_ixyPlusD 2 16])

  -- General-purpose arithmetic and CPU control groups (manual p172)
  , ("00100111",                          [K "daa"])
  , ("00101111",                          [K "cpl"])
  , ("1110110101000100",                  [K "neg"])
  , ("00111111",                          [K "ccf"])
  , ("00110111",                          [K "scf"])
  , ("00000000",                          [K "nop"])
  , ("01110110",                          [K "halt"])
  , ("11110011",                          [K "di"])
  , ("11111011",                          [K "ei"])
  , ("1110110101000110",                  [K "im", K "0"])
  , ("1110110101010110",                  [K "im", K "1"])
  , ("1110110101011110",                  [K "im", K "2"])

  -- 16-bit arithmetic group (manual p187)
  , ("00--1001",                          [K "add", K "hl", Var_ss 2])
  , ("1110110101--1010",                  [K "adc", K "hl", Var_ss 10])
  , ("1110110101--0010",                  [K "sbc", K "hl", Var_ss 10])
  , ("1101110100--1001",                  [K "add", K "ix", Var_pp 10])
  , ("1111110100--1001",                  [K "add", K "iy", Var_rr 10])

  , ("00--0011",                          [K "inc", Var_ss 2])
  , ("11-1110100100011",                  [K "inc", Var_ixy 2])

  , ("00--1011",                          [K "dec", Var_ss 2])
  , ("11-1110100101011",                  [K "dec", Var_ixy 2])

  -- Rotate and shift group (manual p204)
  , ("00000111",                          [K "rlca"])
  , ("00010111",                          [K "rla"])
  , ("00001111",                          [K "rrca"])
  , ("00011111",                          [K "rra"])

  , ("1100101100000---",                  [K "rlc", Var_r 13])
  , ("1100101100000110",                  [K "rlc", Parens_K "hl"])
  , ("11-1110111001011--------00000110",  [K "rlc", Parens_Var_ixyPlusD 2 16])

  , ("1100101100010---",                  [K "rl", Var_r 13])
  , ("1100101100010110",                  [K "rl", Parens_K "hl"])
  , ("11-1110111001011--------00010110",  [K "rl", Parens_Var_ixyPlusD 2 16])

  , ("1100101100001---",                  [K "rrc", Var_r 13])
  , ("1100101100001110",                  [K "rrc", Parens_K "hl"])
  , ("11-1110111001011--------00001110",  [K "rrc", Parens_Var_ixyPlusD 2 16])

  , ("1100101100011---",                  [K "rr", Var_r 13])
  , ("1100101100011110",                  [K "rr", Parens_K "hl"])
  , ("11-1110111001011--------00011110",  [K "rr", Parens_Var_ixyPlusD 2 16])

  , ("1100101100100---",                  [K "sla", Var_r 13])
  , ("1100101100100110",                  [K "sla", Parens_K "hl"])
  , ("11-1110111001011--------00100110",  [K "sla", Parens_Var_ixyPlusD 2 16])

  , ("1100101100101---",                  [K "sra", Var_r 13])
  , ("1100101100101110",                  [K "sra", Parens_K "hl"])
  , ("11-1110111001011--------00101110",  [K "sra", Parens_Var_ixyPlusD 2 16])

  , ("1100101100111---",                  [K "srl", Var_r 13])
  , ("1100101100111110",                  [K "srl", Parens_K "hl"])
  , ("11-1110111001011--------00111110",  [K "srl", Parens_Var_ixyPlusD 2 16])

  , ("1110110101101111",                  [K "rld"])
  , ("1110110101100111",                  [K "rrd"])

  -- Bit set, reset, and test group (manual p242)
  , ("1100101101------",                  [K "bit", Var_b 10, Var_r 13])
  , ("1100101101---110",                  [K "bit", Var_b 10, Parens_K "hl"])
  , ("11-1110111001011--------01---110",  [K "bit", Var_b 26, Parens_Var_ixyPlusD 2 16])

  , ("1100101111------",                  [K "set", Var_b 10, Var_r 13])
  , ("1100101111---110",                  [K "set", Var_b 10, Parens_K "hl"])
  , ("11-1110111001011--------11---110",  [K "set", Var_b 26, Parens_Var_ixyPlusD 2 16])

  , ("1100101110------",                  [K "res", Var_b 10, Var_r 13])
  , ("1100101110---110",                  [K "res", Var_b 10, Parens_K "hl"])
  , ("11-1110111001011--------10---110",  [K "res", Var_b 26, Parens_Var_ixyPlusD 2 16])

  -- Jump group (manual p261)
  , ("11000011----------------",          [K "jp", Var_nn 8])
  , ("11---010----------------",          [K "jp", Var_cc 2, Var_nn 8])
  , ("00011000--------",                  [K "jr", Var_eMinus2 8])
  , ("00111000--------",                  [K "jr", K "c", Var_eMinus2 8])
  , ("00110000--------",                  [K "jr", K "nc", Var_eMinus2 8])
  , ("00101000--------",                  [K "jr", K "z", Var_eMinus2 8])
  , ("00100000--------",                  [K "jr", K "nz", Var_eMinus2 8])
  , ("11101001",                          [K "jp", Parens_K "hl"])
  , ("11-1110111101001",                  [K "jp", Parens_Var_ixy 2])
  , ("00010000--------",                  [K "djnz", Var_eMinus2 8])

  -- Call and return group (manual p280)
  , ("11001101----------------",          [K "call", Var_nn 8])
  , ("11---100----------------",          [K "call", Var_cc 2, Var_nn 8])
  , ("11001001",                          [K "ret"])
  , ("11---000",                          [K "ret", Var_cc 2])
  , ("1110110101001101",                  [K "reti"])
  , ("1110110101000101",                  [K "retn"])
  , ("11---111",                          [K "rst", Var_p 2])

  -- Input and output group (manual p294)
  , ("11011011--------",                  [K "in", K "a", Parens_Var_n 8])
  , ("1110110101---000",                  [K "in", Var_r 10, Parens_K "c"])
  , ("1110110110100010",                  [K "ini"])
  , ("1110110110110010",                  [K "inir"])
  , ("1110110110101010",                  [K "ind"])
  , ("1110110110111010",                  [K "indr"])
  , ("11010011--------",                  [K "out", Parens_Var_n 8, K "a"])
  , ("1110110101---001",                  [K "out", Parens_K "c", Var_r 10])
  , ("1110110110100011",                  [K "outi"])
  , ("1110110110110011",                  [K "otir"])
  , ("1110110110101011",                  [K "outd"])
  , ("1110110110111011",                  [K "otdr"])
  ]


-- | A pairing between a bit pattern and a list of components that comprise an
--   instruction
type InstructionDef = (String, [Component])


-- | A term in an instruction. String parameters are keywords; Int parameters
--   are the bit number where the term's binary representation starts within
--   the assembly of the instruction, where bit 0 is the most significant bit of
--   the first byte of the instruction, however long it may be.
data Component
  = K String                     -- Keyword
  | Parens_K String              -- Keyword inside parentheses
  | Var_b Int                    -- Numbered bit (0-7)
  | Var_n Int                    -- 8-bit constant
  | Var_p Int                    -- RST call parameter (manual p292)
  | Var_r Int                    -- B/C/D/E/H/L/invalid/A (manual p71)
  | Var_ixy Int                  -- IX/IY
  | Var_cc Int                   -- Condition code (manual p263)
  | Var_dd Int                   -- BC/DE/HL/SP as destination (manual p99)
  | Var_nn Int                   -- 16-bit constant
  | Var_pp Int                   -- BC/DE/IX/SP (manual p194)
  | Var_qq Int                   -- BC/DE/HL/AF (manual p115)
  | Var_rr Int                   -- BC/DE/IY/SP (manual p196)
  | Var_ss Int                   -- BC/DE/HL/SP as source (manual p188)
  | Var_eMinus2 Int              -- Relative address, minus 2
  | Parens_Var_ixy Int           -- (IX)/(IY)
  | Parens_Var_ixyPlusD Int Int  -- (IX+d)/(IY+d), 8-bit constant
  | Parens_Var_n Int             -- (n), 8-bit constant
  | Parens_Var_nn Int            -- (nn), 16-bit constant
  deriving (Eq, Show)
