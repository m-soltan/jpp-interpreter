module TopScope where

import Data.Map

import Parsing.AbsLatte
import TopDef

builtins = empty

addId :: Map String Int -> String -> Map String Int
addId m s = insert s (1 + (size m)) m

from :: [TopDef ()] -> Map String Int
from [] = builtins
from ts = case head ts of
  FnDef _ _ (Ident s) _ _ -> addId (from $ tail ts) s

getId :: Map String Int -> String -> Int
getId m s = case Data.Map.lookup s m of
  Just value -> value

