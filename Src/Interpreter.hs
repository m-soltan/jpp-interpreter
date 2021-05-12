module Main where

import Control.Monad      ( when )
import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )

import Parsing.AbsLatte
import Parsing.LexLatte   ( Token )
import Parsing.ParLatte   ( pProgram, myLexer )
import Parsing.PrintLatte ( Print, printTree )
import Parsing.SkelLatte (Err, Result)
import Src.Util
import qualified Src.Program

type ParseFun a = [Token] -> Either String a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run
    fs -> mapM_ runFile fs

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

run :: String -> IO ()
run s = case pProgram ts of
    Right tree -> do
      dbgPrint "\nParse Successful!"
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

transTopDef :: Show a => Parsing.AbsLatte.TopDef a -> String
transTopDef (FnDef _ type_ ident args block) = show type_
