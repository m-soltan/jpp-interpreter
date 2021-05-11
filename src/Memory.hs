module Memory where

import Data.Map

import Parsing.AbsLatte

addFunction :: Map Int (TopDef ()) -> (TopDef ()) -> Map Int (TopDef ())
addFunction m fun = insert (1 + (size m)) fun m

getFunction :: Map Int (TopDef ()) -> Int -> (TopDef ())
getFunction m cell = case Data.Map.lookup cell m of
    Just v -> v
