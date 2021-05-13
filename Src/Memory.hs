module Src.Memory where

import Data.Map

import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte ( Err, Result )
import Src.Util ( (|>) )

addError :: String -> MemoryState a -> MemoryState a
addError err m = case (except m, retVal m) of
  (Right "ok", Nothing) -> MemoryState {
    funcs = funcs m,
    vIdent = vIdent m,
    vStore = vStore m,
    except = Left err,
    retVal = retVal m
  }

addFunction :: (TopDef a) -> MemoryState a -> MemoryState a
addFunction fun m = case fun of
  (FnDef _ _ (Ident str) _ _) -> case (except m, retVal m) of
    (Right "ok", Nothing) -> MemoryState {
      funcs = m |> funcs |> insert str (Just fun),
      vIdent = vIdent m,
      vStore = vStore m,
      except = except m,
      retVal = retVal m
    }

addRetVal :: MemoryState a -> MemoryState a
addRetVal m = case (except m, retVal m) of
  (Right "ok", Nothing) -> case vRead "0" m of
    Just x -> MemoryState {
      funcs = funcs m,
      vIdent = vIdent m,
      vStore = vStore m,
      except = except m,
      retVal = Just x
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
  except = Right "ok",
  retVal = Nothing
}

fromDefs :: [TopDef a] -> MemoryState a
fromDefs [] = emptyState
fromDefs (h : t) = fromDefs t |> addFunction h

functionScope :: MemoryState a -> MemoryState a
functionScope m = case except m of
  Right "ok" -> MemoryState {
    funcs = funcs m,
    vIdent = empty,
    vStore = empty,
    except = Right "ok",
    retVal = Nothing
  }

getFunction :: String -> MemoryState a -> Maybe (TopDef a)
getFunction ident m = case m |> funcs |> Data.Map.lookup ident of
    Just (Just v) -> Just v
    Nothing -> Nothing

-- store the immediate result or return value
vHold :: MemoryValue -> MemoryState a -> IO (MemoryState a)
vHold = vDeclare "0"

vDeclare :: String -> MemoryValue -> MemoryState a -> IO (MemoryState a)
vDeclare ident v m = do
  let sz = m |> vIdent |> size
  return MemoryState {
    funcs = funcs m,
    vIdent = m |> vIdent |> insert ident sz,
    vStore = m |> vStore |> insert sz v,
    except = except m,
    retVal = retVal m
  }

vRead :: String -> MemoryState a -> Maybe MemoryValue
vRead ident m = case m |> vIdent |> Data.Map.lookup ident of
  Just cell -> m |> vStore |> Data.Map.lookup cell
  Nothing -> Nothing

-- get the held value
vUnhold :: MemoryState a -> MemoryValue
vUnhold = case vRead "0" of
  Just v -> v

data MemoryState a = MemoryState {
  funcs :: Map String (Maybe (TopDef a)),
  vIdent :: Map String Int,
  vStore :: Map Int MemoryValue,
  except :: Result,
  -- functionn return value
  retVal :: Maybe MemoryValue
}

data MemoryValue = ValStr String | ValInt Integer | ValBool Bool
