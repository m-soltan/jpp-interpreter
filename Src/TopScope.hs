module Src.TopScope where

import Data.Map

import Parsing.AbsLatte
import TopDef
import Src.Util ( (|>) )

builtins = empty

addId :: String -> Map String Int -> Map String Int
addId s m = insert s (1 + (size m)) m

from :: [TopDef ()] -> Map String Int
from [] = builtins
from ts = case head ts of
  FnDef _ _ (Ident s) _ _ -> ts
    |> tail
    |> from
    |> addId s

getId :: String -> Map String Int -> Int
getId s m = case Data.Map.lookup s m of
  Just value -> value
