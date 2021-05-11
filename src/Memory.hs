module Memory where

import Data.Map

import Parsing.AbsLatte
import Parsing.SkelLatte ( Err, Result )
import Util ( (|>) )

addFunction :: (TopDef ()) -> MemoryState -> MemoryState
addFunction fun m = case fun of
  (FnDef _ _ (Ident str) _ _) -> MemoryState {
  funcs = m |> funcs |> insert str fun,
  vIdent = vIdent m,
  vStore = vStore m,
  except = except m
}

emptyState :: MemoryState
emptyState = MemoryState {
  funcs = empty,
  vIdent = empty,
  vStore = empty,
  except = Right "ok"
}

fromDefs :: [TopDef ()] -> MemoryState
fromDefs [] = emptyState
fromDefs (h : t) = fromDefs t |> addFunction h

getFunction :: MemoryState -> String -> TopDef ()
getFunction m cell = case m |> funcs |> Data.Map.lookup cell of
    Just v -> v

data MemoryState = MemoryState {
  funcs :: Map String (TopDef ()),
  vIdent :: Map String Int,
  vStore :: Map Int (Parsing.AbsLatte.Type ()),
  except :: Result
}
