module Src.TopDef where

import Src.Memory
import Parsing.AbsLatte
import Parsing.SkelLatte (Err, Result)
import Src.Block

trans :: Parsing.AbsLatte.TopDef () -> MemoryState -> IO Result
trans (FnDef _ type_ ident args block) m = do
  return (Left "not implemented")

callFunc :: Parsing.AbsLatte.TopDef () -> MemoryState -> IO (MemoryState)
callFunc f m = case f of
  FnDef _ type_ (Ident k) args block -> do
    -- putStrLn (show l)
    putStrLn k
    return m
