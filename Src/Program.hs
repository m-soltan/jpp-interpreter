module Src.Program where

import Data.Map

import Src.Memory
import Parsing.AbsLatte
import Parsing.LexLatte   ( Token )
import Parsing.SkelLatte ( Err, Result )
import Src.TopDef
import Src.Util ( (|>) )

trans :: Parsing.AbsLatte.Program () -> IO Result
trans (Program () l) = do
  let defs = fromDefs l
  after <- callMain defs
  let retCode = except after
  return retCode

callMain :: MemoryState -> IO MemoryState
callMain m = case m |> funcs |> Data.Map.lookup "main" of
  Just f -> callFunc f m
