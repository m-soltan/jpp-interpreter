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
    vGlobal = vGlobal m,
    vStore = vStore m,
    ans = ans m,
    except = Left err,
    retVal = retVal m
  }

addFunction :: (TopDef a) -> MemoryState a -> MemoryState a
addFunction fun m = case fun of
  (FnDef _ _ (Ident str) _ _) -> case (except m, retVal m) of
    (Right "ok", Nothing) -> MemoryState {
      funcs = m |> funcs |> insert str (Just fun),
      vIdent = vIdent m,
      vGlobal = vGlobal m,
      vStore = vStore m,
      ans = ans m,
      except = except m,
      retVal = retVal m
    }

addRetVal :: MemoryState a -> MemoryState a
addRetVal m = case (except m, retVal m) of
  (Right "ok", _) -> MemoryState {
      funcs = funcs m,
      vIdent = vIdent m,
      vGlobal = vGlobal m,
      vStore = vStore m,
      ans = ans m,
      except = except m,
      retVal = m |> vUnhold |> Just
    }

blockScope :: MemoryState a -> MemoryState a
blockScope m = case (except m, retVal m) of
  (Right "ok", Nothing) -> MemoryState {
    funcs = funcs m,
    vIdent = empty,
    vGlobal = union (vIdent m) (vGlobal m),
    vStore = vStore m,
    ans = ans m,
    except = Right "ok",
    retVal = Nothing
  }

setLocal :: (Map String (Type a, Int)) -> MemoryState a -> MemoryState a
setLocal local m = MemoryState {
  funcs = funcs m,
  vIdent = vIdent m,
  vGlobal = local,
  vStore = vStore m,
  ans = ans m,
  except = except m,
  retVal = retVal m
}

builtins :: Map String (Maybe (TopDef a))
builtins = empty
 |> insert "fail" Nothing
 |> insert "printString" Nothing

emptyState :: a -> MemoryState a
emptyState a = MemoryState {
  funcs = builtins,
  vIdent = empty,
  vGlobal = empty,
  vStore = empty,
  ans = Nothing,
  except = Right "ok",
  retVal = Nothing
}

fromDefs :: a -> [TopDef a] -> MemoryState a
fromDefs a [] = emptyState a
fromDefs a (h : t) = fromDefs a t |> addFunction h

fromScope :: MemoryState a -> MemoryState a -> MemoryState a
fromScope inner outer = MemoryState {
  funcs = funcs outer,
  vIdent = vIdent outer,
  vGlobal = vGlobal outer,
  vStore = vStore inner,
  ans = ans inner,
  except = except inner,
  retVal = retVal inner
}

functionScope :: MemoryState a -> MemoryState a
functionScope m = case except m of
  Right "ok" -> MemoryState {
    funcs = funcs m,
    vIdent = empty,
    vGlobal = empty,
    vStore = empty,
    ans = Nothing,
    except = Right "ok",
    retVal = Nothing
  }

getFreeCell :: MemoryState a -> Int
getFreeCell m = size (vStore m)

getFunction :: String -> MemoryState a -> Maybe (TopDef a)
getFunction ident m = case m |> funcs |> Data.Map.lookup ident of
    Just (Just v) -> Just v
    Nothing -> Nothing

typeOf :: MemoryValue a -> Type a
typeOf (ValStr a _) = Str a
typeOf (ValInt a _) = Int a
typeOf (ValBool a _) = Bool a
typeOf (ValVoid a) = Void a

vAssign :: String -> MemoryValue a -> MemoryState a -> IO (MemoryState a)
vAssign ident v m = case m |> vIdent |> Data.Map.lookup ident of
  Just (t, cell) -> do
    let newMap = vReplace ident v m
    case (v, t) of
      (ValStr _ _, Str _) -> return newMap
      (ValInt _ _, Int _) -> return newMap
      (ValBool _ _, Bool _) -> return newMap

vDeclare :: String -> Type a -> MemoryState a -> IO (MemoryState a)
vDeclare ident t m = do
  let sz = getFreeCell m
  return MemoryState {
      funcs = funcs m,
      vIdent = m |> vIdent |> insert ident (t, sz),
      vGlobal = vGlobal m,
      vStore = vStore m,
      ans = ans m,
      except = except m,
      retVal = retVal m
    }

vDelete :: String -> MemoryState a -> MemoryState a
vDelete s m = MemoryState {
  funcs = funcs m,
  vIdent = m |> vIdent |> delete s,
  vGlobal = vGlobal m,
  vStore = vStore m,
  ans = ans m,
  except = except m,
  retVal = retVal m
}

vGet :: String -> MemoryState a -> Maybe (Type a, Int)
vGet ident m = case m |> vIdent |> Data.Map.lookup ident of
  Just v -> Just v
  Nothing -> m |> vGlobal |> Data.Map.lookup ident

vInitialize :: String -> MemoryValue a -> MemoryState a -> IO (MemoryState a)
vInitialize ident v m = case m |> vIdent |> Data.Map.lookup ident of
  Just _ -> return (addError ("redeclaration of variable " ++ ident) m)
  Nothing -> do
    let sz = getFreeCell m
    return MemoryState {
      funcs = funcs m,
      vIdent = m |> vIdent |> insert ident (typeOf v, sz),
      vGlobal = vGlobal m,
      vStore = m |> vStore |> insert sz v,
      ans = ans m,
      except = except m,
      retVal = retVal m
    }

vGetType :: String -> MemoryState a -> Maybe (Type a)
vGetType ident m = case m |> vIdent |> Data.Map.lookup ident of
  Just (t, _) -> Just t
  Nothing -> Nothing

-- store the immediate result or return value
vHold :: MemoryValue a -> MemoryState a -> IO (MemoryState a)
vHold v m = do
  let m1 = MemoryState {
    funcs = funcs m,
    vIdent = vIdent m,
    vGlobal = vGlobal m,
    vStore = vStore m,
    ans = Just v,
    except = except m,
    retVal = retVal m
  }
  return m1

vRead :: String -> MemoryState a -> Maybe (MemoryValue a)
vRead ident m = case vGet ident m of
  Just (_, i) -> Data.Map.lookup i (vStore m)
  Nothing -> Nothing

vReplace :: String -> MemoryValue a -> MemoryState a -> MemoryState a
vReplace ident v m = case vGet ident m of
  Just (_, i) -> MemoryState {
    funcs = funcs m,
    vIdent = vIdent m,
    vGlobal = vGlobal m,
    vStore = m |> vStore |> insert i v,
    ans = ans m,
    except = except m,
    retVal = retVal m 
  }

-- get the held value
vUnhold :: MemoryState a -> MemoryValue a
vUnhold m = case ans m of
  Just v -> v

data MemoryState a = MemoryState {
  funcs :: Map String (Maybe (TopDef a)),
  vIdent :: Map String (Type a, Int),
  vGlobal :: Map String (Type a, Int),
  vStore :: Map Int (MemoryValue a),
  ans :: Maybe (MemoryValue a),
  except :: Result,
  -- function return value
  retVal :: Maybe (MemoryValue a)
}

data MemoryValue a = ValStr a String | ValInt a Integer | ValBool a Bool | ValVoid a
