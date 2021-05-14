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
    vLocal = vLocal m,
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
      vLocal = vLocal m,
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
      vLocal = vLocal m,
      vStore = vStore m,
      ans = ans m,
      except = except m,
      retVal = m |> vUnhold |> Just
    }

blockScope :: MemoryState a -> MemoryState a
blockScope m = case (except m, retVal m) of
  (Right "ok", Nothing) -> MemoryState {
    funcs = funcs m,
    vIdent = vIdent m,
    vLocal = empty,
    vStore = vStore m,
    ans = ans m,
    except = except m,
    retVal = retVal m
  }

setLocal :: (Map String ()) -> MemoryState a -> MemoryState a
setLocal local m = MemoryState {
  funcs = funcs m,
  vIdent = vIdent m,
  vLocal = local,
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
  vLocal = empty,
  vStore = empty,
  ans = Nothing,
  except = Right "ok",
  retVal = Nothing
}

fromDefs :: a -> [TopDef a] -> MemoryState a
fromDefs a [] = emptyState a
fromDefs a (h : t) = fromDefs a t |> addFunction h

functionScope :: MemoryState a -> MemoryState a
functionScope m = case except m of
  Right "ok" -> MemoryState {
    funcs = funcs m,
    vIdent = empty,
    vLocal = empty,
    vStore = empty,
    ans = Nothing,
    except = Right "ok",
    retVal = Nothing
  }

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
  let sz = m |> vIdent |> size
  return MemoryState {
      funcs = funcs m,
      vIdent = m |> vIdent |> insert ident (t, sz),
      vLocal = vLocal m,
      vStore = vStore m,
      ans = ans m,
      except = except m,
      retVal = retVal m
    }

vDelete :: String -> MemoryState a -> MemoryState a
vDelete s m = MemoryState {
  funcs = funcs m,
  vIdent = m |> vIdent |> delete s,
  vLocal = vLocal m,
  vStore = vStore m,
  ans = ans m,
  except = except m,
  retVal = retVal m
}

vInitialize :: String -> MemoryValue a -> MemoryState a -> IO (MemoryState a)
vInitialize ident v m = case m |> vIdent |> Data.Map.lookup ident of
  Just _ -> return (addError ("redeclaration of variable " ++ ident) m)
  Nothing -> do
    let sz = m |> vIdent |> size
    return MemoryState {
      funcs = funcs m,
      vIdent = m |> vIdent |> insert ident (typeOf v, sz),
      vLocal = vLocal m,
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
    vLocal = vLocal m,
    vStore = vStore m,
    ans = Just v,
    except = except m,
    retVal = retVal m
  }
  return m1

vRead :: String -> MemoryState a -> Maybe (MemoryValue a)
vRead ident m = case m |> vIdent |> Data.Map.lookup ident of
  Just (_, cell) -> m |> vStore |> Data.Map.lookup cell
  Nothing -> Nothing

vReplace :: String -> MemoryValue a -> MemoryState a -> MemoryState a
vReplace ident v m = case m |> vIdent |> Data.Map.lookup ident of
  Just (_, i) -> MemoryState {
    funcs = funcs m,
    vIdent = vIdent m,
    vLocal = vLocal m,
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
  vLocal :: Map String (),
  vStore :: Map Int (MemoryValue a),
  ans :: Maybe (MemoryValue a),
  except :: Result,
  -- functionn return value
  retVal :: Maybe (MemoryValue a)
}

data IdsState a = Global a (Map String (Type a, Int))
    | Local a (Map String (Type a, Int)) (IdsState a)

data MemoryValue a = ValStr a String | ValInt a Integer | ValBool a Bool | ValVoid a
