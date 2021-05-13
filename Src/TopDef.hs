module Src.TopDef where

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte (Err, Result)
import Src.Util ( (|>) )

transTopDef :: Src.Parsing.AbsLatte.TopDef a -> MemoryState a -> IO Result
transTopDef (FnDef _ type_ ident args block) m = do
  return (Left "not implemented")

callFunc :: Src.Parsing.AbsLatte.TopDef a -> MemoryState a -> IO (MemoryState a)
callFunc f m = case f of
  FnDef _ type_ (Ident k) args block -> do
    callResult <- transBlock block (functionScope m)
    case except callResult of
      Right "ok" -> case vRead "0" callResult of
        Just v -> do
          m <- vDeclare "0" v m
          return m
        Nothing -> do
          let m1 = addError "function call returned nothing" m
          return m1
      Left err -> do
        return (addError err m)

transBlock :: Src.Parsing.AbsLatte.Block a -> MemoryState a -> IO (MemoryState a)
transBlock (Block a l) m = do
  case l of
    [] -> return m
    h : t -> do
      m <- transStmt h m
      case (except m, retVal m) of
        (Right "ok", Nothing) -> do
          transBlock (Block a t) m
        _ -> return m

transStmt :: Stmt a -> MemoryState a -> IO (MemoryState a)
transStmt (Empty _) m = do
  return m
transStmt (BStmt _ b) m = do
  let m1 = addError "block statement not implemented" m
  return m1
transStmt (Decl _ _ _) m = do
  let m1 = addError "declaration statement not implemented" m
  return m1
transStmt (Ass _ _ _) m = do
  let m1 = addError "assignment statement not implemented" m
  return m1
transStmt (Incr _ _) m = do
  let m1 = addError "increment statement not implemented" m
  return m1
transStmt (Decr _ _) m = do
  let m1 = addError "decrement statement not implemented" m
  return m1
transStmt (Ret _ e) m = do
  m <- transExpr e m
  let m1 = addRetVal m
  return m1
transStmt (VRet _) m = do
  let m1 = addError "variable return statement not implemented" m
  return m1
transStmt (Cond _ e s) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vRead "0" m of
      Just (ValBool True) -> do
        m <- transStmt s m
        return m
      Just (ValBool False) -> do
        return m
      Just _ -> do
        let m1 = addError "non-boolean condition in if statement" m
        return m1
    Left err -> do
      return m
transStmt (CondElse _ _ _ _) m = do
  let m1 = addError "if-else statement not implemented" m
  return m1
transStmt (While _ _ _) m = do
  let m1 = addError "while loop not implemented" m
  return m1
transStmt (SExp _ e) m = do
  m <- transExpr e m
  return m


transExpr :: Expr a -> MemoryState a -> IO (MemoryState a)
transExpr (EVar _ _) m = do
  let m1 = addError "variable expressions not implemented" m
  return m1
transExpr (ELitInt _ i) m = do
  m <- vDeclare "0" (ValInt i) m
  return m
transExpr (ELitTrue _) m = do
  m <- vDeclare "0" (ValBool True) m
  return m
transExpr (ELitFalse _) m = do
  m <- vDeclare "0" (ValBool False) m
  return m
transExpr (EApp _ (Ident ident) l) m = case ident of
  "fail" -> do
    case l of
      [EString _ str] -> do
        let m1 = addError str m
        return m1
  "printString" -> do
    case l of
      [EString _ str] -> do
        putStrLn str
        return m
  _ -> do
    let r = getFunction ident m
    case r of
      Just f -> callFunc f m
      Nothing -> do
        let m1 = addError "call to undefined function" m
        return m1
transExpr (EString _ str) m = do
  m <- vDeclare "0" (ValStr str) m
  return m
transExpr (Neg _ e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vRead "0" m of
      Just v -> case v of
        ValInt i -> do
          let newValue = ValInt (-i)
          m <- vDeclare "0" newValue m
          return m
        _ -> do
          let m1 = addError "invalid type passed to arithmetic negation" m
          return m1
    Left err -> return m
transExpr (Not _ e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vRead "0" m of
      Just v -> case v of
        ValBool b -> do
          let newValue = ValBool (not b)
          m <- vDeclare "0" newValue m
          return m
        _ -> do
          let m1 = addError "invalid type passed to logical negation" m
          return m1
    Left err -> return m
transExpr (EMul _ _ _ _) m = do
  let m1 = addError "multiplication in expressions not implemented" m
  return m1
transExpr (EAdd _ _ _ _) m = do
  let m1 = addError "addition in expressions not implemented" m
  return m1
transExpr (ERel _ _ _ _) m = do
  let m1 = addError "comparison in expressions not implemented" m
  return m1
transExpr (EAnd _ _ _) m = do
  let m1 = addError "logical \"and\" in expressions not implemented" m
  return m1
transExpr (EOr _ _ _) m = do
  let m1 = addError "logical \"or\" in expressions not implemented" m
  return m1
