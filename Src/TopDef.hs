module Src.TopDef where

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte (Err, Result)
import Src.Block

trans :: Src.Parsing.AbsLatte.TopDef () -> MemoryState -> IO Result
trans (FnDef _ type_ ident args block) m = do
  return (Left "not implemented")

callFunc :: Src.Parsing.AbsLatte.TopDef () -> MemoryState -> IO (MemoryState)
callFunc f m = case f of
  FnDef _ type_ (Ident k) args block -> do
    putStrLn k
    return m
