module TopDef where

import Memory
import Parsing.AbsLatte
import Parsing.SkelLatte (Err, Result)

trans :: Parsing.AbsLatte.TopDef () -> MemoryState -> IO Result
trans (FnDef _ type_ ident args block) m = do
  return (Left "not implemented")

callFunc :: Parsing.AbsLatte.TopDef () -> MemoryState -> IO (MemoryState)
callFunc f m = case f of
  FnDef _ type_ (Ident "printString") args block -> do
    putStrLn "test"
    return m
  FnDef _ type_ ident args block -> do
    putStrLn "test"
    return m
