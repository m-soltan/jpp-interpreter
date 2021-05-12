module Main where

import Control.Monad      ( when )
import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )

import Src.Parsing.AbsLatte
import Src.Parsing.LexLatte   ( Token )
import Src.Parsing.ParLatte   ( pProgram, myLexer )
import Src.Parsing.SkelLatte (Err, Result)
import Src.Util
import qualified Src.Program

type ParseFun a = [Token] -> Either String a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run
    fs -> mapM_ runFile fs

run :: String -> IO ()
run s = case pProgram ts of
    Right tree -> do
      dbgPrint "\nParse Successful!"
      dbgPrint (show tree)
      programResult <- Src.Program.trans tree
      case programResult of
        Right s -> do
          dbgPrint s
          exitSuccess
        Left err -> do
          dbgPrint ("\nRuntime error\n" ++ err)
          exitFailure
    Left s -> do
      dbgPrint "\nParse Failed...\n"

      exitFailure
  where
  ts = myLexer s

runFile :: FilePath -> IO ()
runFile f = readFile f >>= run
