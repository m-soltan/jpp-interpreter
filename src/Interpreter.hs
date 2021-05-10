module Main where

import Control.Monad      ( when )
import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )

import Parsing.LexLatte   ( Token )
import Parsing.ParLatte   ( pProgram, myLexer )
import Parsing.PrintLatte ( Print, printTree )

type ParseFun a = [Token] -> Either String a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run 2 pProgram

run :: (Print a, Show a) => Int -> ParseFun a -> String -> IO ()
run v p s = case p ts of
    Left s -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn s
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"

      exitSuccess
  where
  ts = myLexer s

putStrV :: Int -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s