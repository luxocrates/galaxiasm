-- Top-level function for executable, and the only non-pure support functions
-- for the dis/assembler.

import Data.ByteString (pack, readFile, unpack, writeFile)
import Data.List (intercalate, sortBy)
import Data.Map (toList)
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Exit (die)
import System.FilePath (takeBaseName, takeFileName, takeDirectory, (</>))
import System.IO (hPutStr, readFile, writeFile, stderr)

import qualified Asm.Assemble (assemble)
import Asm.Rom (segment)
import CommandLine
  ( CliConfig (Assemble, Disassemble, ShowHelp)
  , RomFormat (..)
  , parseCommandLine
  , showHelp
  )
import Common.String (showHex4)
import qualified Disasm.Disassemble (disassemble)

import Asm.CharStream (makeCs)
import Asm.Parsers.Line (parseInclude)

import Asm.Types.LineAndSource (LineAndSource)
import Asm.Types.Parser (ParseAttempt (ParseError, ParseNearly, ParseSuccess))
import Asm.Types.Pass2 (Symbol, SymTable, SymbolType(Label))

outDirName :: String
outDirName = "out"

symFilePath :: String
symFilePath = outDirName </> "syms"

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

-- | Converts a single source line into a list of source lines, by recursively
--   expanding any `.include` directives.
expandIncludes
  :: LineAndSource  -- a fresh line
  -> String         -- implicit path
  -> [String]       -- parent filenames found so far
  -> IO [LineAndSource]

expandIncludes lineAndSource implicitPath parents =
  let
    (line, source) = lineAndSource
    (filename, _) = source
  in
    case parseInclude (makeCs line source) of
      ParseNearly _
        -> return [lineAndSource]
      ParseError _  -- TODO: consider using this for the no-end-quote case
        -> return [lineAndSource]
      ParseSuccess (_, incFilename)
        -> loadWithIncludes incFilename implicitPath (filename:parents)


-- | Loads a file, recursing into all `.include` dependencies, emitting the
--   complete program as a list source lines with provenance attribution
loadWithIncludes
  :: String   -- input filename
  -> String   -- implicit path
  -> [String] -- parent files
  -> IO [LineAndSource]

loadWithIncludes filename implicitPath parents =
  if (filename `elem` parents)
  then error $ "Circular dependency when including '" ++ filename ++ "'"
  else do
    contents <- System.IO.readFile $ implicitPath </> filename

    let attribLines = zip
                        (lines contents)
                        (map (\lineNum -> (filename, lineNum)) [1..])

    listOfLists <- sequence $
                     map
                      (\x -> (expandIncludes x implicitPath parents))
                      attribLines

    return $ concat listOfLists


-- | Load a file, run it through the pure assembler, emit its output, or print
--   errors
assemble
  :: String     -- input filename
  -> Bool       -- emit symbols
  -> RomFormat  -- output format
  -> IO [()]    -- IO for file writes and stdout/stderr printing
assemble inFilename emitSyms format = do
  -- TODO: catch file read errors
  contents <- loadWithIncludes
    (takeFileName inFilename)
    (takeDirectory inFilename)
    []
  case Asm.Assemble.assemble contents of
    Left err
      -> sequence [die err]
    Right arts
      -> do
        let (bs, (_, (symTable, _, _))) = arts

        createDirectoryIfMissing False outDirName
        sequence $
          concatMap (\(romName, start, padding, w8s) ->
            let filename = outDirName </> romName in
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
            ]) (segment bs format (takeBaseName inFilename))
            ++ (if emitSyms then emitSymsFile symTable else [])


-- | Main entrypoint for command-line executable
main :: IO [()]
main = do
  argv <- getArgs
  case parseCommandLine argv of
    Left err                              -> sequence [hPutStr stderr $ err ++ "\n"]
    Right (Disassemble filename)          -> sequence [disassemble filename]
    Right (Assemble filename syms format) -> assemble filename syms format
    Right ShowHelp                        -> sequence [putStr showHelp]
