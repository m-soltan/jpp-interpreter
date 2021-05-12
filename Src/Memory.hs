module Src.Memory where

import Data.Map

import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte ( Err, Result )
import Src.Util ( (|>) )

addError :: String -> MemoryState a -> MemoryState a
addError err m = MemoryState {
  funcs = funcs m,
  vIdent = vIdent m,
  vStore = vStore m,
  except = Left err
}

addFunction :: (TopDef a) -> MemoryState a -> MemoryState a
addFunction fun m = case fun of
  (FnDef _ _ (Ident str) _ _) -> MemoryState {
  funcs = m |> funcs |> insert str (Just fun),
  vIdent = vIdent m,
  vStore = vStore m,
  except = except m
}

builtins :: Map String (Maybe (TopDef a))
builtins = empty
 |> insert "fail" Nothing
 |> insert "printString" Nothing

emptyState :: MemoryState a
emptyState = MemoryState {
  funcs = builtins,
  vIdent = empty,
  vStore = empty,
  except = Right "ok"
}

fromDefs :: [TopDef a] -> MemoryState a
fromDefs [] = emptyState
fromDefs (h : t) = fromDefs t |> addFunction h

getFunction :: String -> MemoryState a -> Maybe (TopDef a)
getFunction ident m = case m |> funcs |> Data.Map.lookup ident of
    Just (Just v) -> Just v
    Nothing -> Nothing

vDeclare :: String -> MemoryValue -> MemoryState a -> IO (MemoryState a)
vDeclare ident v m = do
  let sz = m |> vIdent |> size
  return MemoryState {
    funcs = funcs m,
    vIdent = m |> vIdent |> insert ident sz,
    vStore = m |> vStore,
    except = except m
  }

vRead :: String -> MemoryState a -> Maybe MemoryValue
vRead ident m = case m |> vIdent |> Data.Map.lookup ident of
  Just cell -> m |> vStore |> Data.Map.lookup cell
  Nothing -> Nothing


data MemoryState a = MemoryState {
  funcs :: Map String (Maybe (TopDef a)),
  vIdent :: Map String Int,
  vStore :: Map Int MemoryValue,
  except :: Result
}

data MemoryValue = ValStr String | ValInt Integer | ValBool Bool
