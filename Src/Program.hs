module Src.Program where

import Data.Map

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.LexLatte   ( Token )
import Src.Parsing.SkelLatte ( Err, Result )
import Src.TopDef
import Src.Util ( (|>) )

trans :: Src.Parsing.AbsLatte.Program a -> IO Result
trans (Program a l) = do
  let defs = fromDefs a l
  after <- callMain defs
  let retCode = except after
  return retCode

callMain :: MemoryState a -> IO (MemoryState a)
callMain m = do
  let r = getFunction "main" m
  case r of
    Just f -> callFunc f [] m
    Nothing -> do
      let m1 = addError "missing declaration of main()" m
      return m1
