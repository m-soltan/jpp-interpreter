module Main where

import Control.Monad      ( when )
import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )

import Parsing.AbsLatte
import Parsing.LexLatte   ( Token )
import Parsing.ParLatte   ( pProgram, myLexer )
import Parsing.PrintLatte ( Print, printTree )
import Parsing.SkelLatte (Err, Result)
import qualified Program

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
      debugPrint "\nParse Successful!"
      case Program.trans tree of
        Right s -> do
          debugPrint s
          exitSuccess
        Left err -> do
          debugPrint ("\nRuntime error\n" ++ err)
          exitFailure
    Left s -> do
      debugPrint "\nParse Failed...\n"

      exitFailure
  where
  ts = myLexer s

transTopDef :: Show a => Parsing.AbsLatte.TopDef a -> String
transTopDef (FnDef _ type_ ident args block) = show type_

debugPrint = putStrLn