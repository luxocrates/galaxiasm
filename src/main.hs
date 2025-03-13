-- Top-level function for executable, and the only non-pure support functions
-- for the dis/assembler.

import Data.ByteString (pack, readFile, unpack, writeFile)
import Data.List (intercalate, sortBy)
import Data.Map (toList)
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Exit (die)
import System.IO (hPutStr, readFile, writeFile, stderr)

import qualified Asm.Assemble (assemble)
import Asm.Rom (segment)
import CommandLine
  ( CliConfig (Assemble, Disassemble, ShowHelp)
  , parseCommandLine
  , showHelp
  )
import Common.String (showHex4)
import qualified Disasm.Disassemble (disassemble)

import Asm.Types.Pass2 (Symbol, SymTable, SymbolType(Label))


outDirName :: String
outDirName = "out"

symFilePath :: String
symFilePath = outDirName ++ "/" ++ "syms"

-- | Load a file, run it through the pure disassembler, print its output
disassemble
  :: String  -- input filename
  -> IO ()   -- IO for stdout printing
disassemble filename = do
  -- TODO: catch file read errors
  contents <- Data.ByteString.readFile filename
  putStrLn $ Disasm.Disassemble.disassemble $ unpack contents


symTableToStrings :: SymTable -> [String]
symTableToStrings table = map tableEntryToString sortedFilteredSyms
  where
    sortedFilteredSyms = sortBy compareSyms filteredSyms
    filteredSyms = filter justLabels (toList table)

    symToAddrString :: Symbol -> String
    symToAddrString (val, _) = showHex4 val

    tableEntryToString :: (String, Symbol) -> String
    tableEntryToString (name, sym) = name ++ ": " ++ symToAddrString sym

    justLabels :: (String, Symbol) -> Bool
    justLabels (_, (_, (Label, _))) = True
    justLabels _                    = False

    compareSyms :: (String, Symbol) -> (String, Symbol) -> Ordering
    compareSyms (_, (addr1, _)) (_, (addr2, _)) | addr1 < addr2 = LT
                                                | otherwise     = GT


emitSymsFile :: SymTable -> [IO ()]
emitSymsFile symTable = [
  System.IO.writeFile symFilePath $ intercalate "\n" lines,
  putStrLn $ "Wrote syms table to " ++ symFilePath
  ]
  where
    lines = symTableToStrings symTable ++ [""]


-- | Load a file, run it through the pure assembler, emit its output, or print
--   errors
assemble
  :: String   -- input filename
  -> Bool     -- emit symbols
  -> IO [()]  -- IO for file writes and stdout/stderr printing
assemble inFilename emitSyms = do
  -- TODO: catch file read errors
  contents <- System.IO.readFile inFilename
  case Asm.Assemble.assemble contents inFilename of
    Left err
      -> sequence [die err]
    Right arts
      -> do
        let (bs, (_, (symTable, _, _))) = arts

        createDirectoryIfMissing False outDirName
        sequence $
          concatMap (\(romName, start, padding, w8s) ->
            let filename = outDirName ++ "/" ++ romName in
            [ Data.ByteString.writeFile filename (pack w8s)
            , putStrLn $
              "Wrote " ++ showHex4 start
              ++ "-" ++ showHex4 (start + length w8s)
              ++ " to " ++ filename
              ++ (
                if padding > 0
                  then " (with " ++ show padding ++ " bytes padding)"
                  else ""
                )
            ]) (segment bs)
            ++ (if emitSyms then emitSymsFile symTable else [])


-- | Main entrypoint for command-line executable
main :: IO [()]
main = do
  argv <- getArgs
  case parseCommandLine argv of
    Left err                       -> sequence [hPutStr stderr $ err ++ "\n"]
    Right (Disassemble filename)   -> sequence [disassemble filename]
    Right (Assemble filename syms) -> assemble filename syms
    Right ShowHelp                 -> sequence [putStr showHelp]
