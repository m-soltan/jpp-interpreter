module Src.Program where

import Data.Map

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.LexLatte   ( Token )
import Src.Parsing.SkelLatte ( Err, Result )
import Src.TopDef
import Src.Util ( (|>) )

trans :: Src.Parsing.AbsLatte.Program () -> IO Result
trans (Program () l) = do
  let defs = fromDefs l
  after <- callMain defs
  let retCode = except after
  return retCode

callMain :: MemoryState a -> IO (MemoryState a)
callMain m = do
  let f = getFunction "main" m
  callFunc f m
