module Src.Block where

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte (Err, Result)

trans :: Src.Parsing.AbsLatte.Block () -> MemoryState -> IO Result
trans (Block _ l) m = do
  case l of
    [] -> return (Left "not implemented")
    h : t -> do
      trans (Block () t) m
