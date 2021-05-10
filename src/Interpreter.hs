module Main where

import Control.Monad      ( when )
import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )

import Parsing.AbsLatte
import Parsing.LexLatte   ( Token )
import Parsing.ParLatte   ( pProgram, myLexer )
import Parsing.PrintLatte ( Print, printTree )
-- import Parsing.SkelLatte

type ParseFun a = [Token] -> Either String a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run
    fs -> mapM_ runFile fs

run :: String -> IO ()
run s = case pProgram ts of
    Left s -> do
      putStrLn "\nParse Failed...\n"

      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      case transProgram tree of
        s -> do
          putStrLn s
      -- transProgram2 tree
      exitSuccess
  where
  ts = myLexer s

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

transProgram :: Parsing.AbsLatte.Program () -> String
transProgram (Program () l) = show l

transTopDef :: Show a => Parsing.AbsLatte.TopDef a -> String
transTopDef (FnDef _ type_ ident args block) = show type_
