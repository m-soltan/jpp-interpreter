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
    m <- transBlock block m
    return m

transBlock :: Src.Parsing.AbsLatte.Block a -> MemoryState a -> IO (MemoryState a)
transBlock (Block a l) m = do
  case l of
    [] -> return m
    h : t -> do
      m <- transStmt h m
      transBlock (Block a t) m

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
  return m
transStmt (VRet _) m = do
  let m1 = addError "variable return statement not implemented" m
  return m1
transStmt (Cond _ _ _) m = do
  let m1 = addError "conditional statement not implemented" m
  return m1
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
transExpr (EApp _ (Ident ident) l) m = case ident of
  "fail" -> do
    case l of
      [EString _ str] -> do
        let m = addError str m
        return m
  "printString" -> do
    case l of
      [EString _ str] -> do
        putStrLn str
        return m
  _ -> do
    let f = getFunction ident m
    callFunc f m
transExpr (ELitInt _ i) m = do
  m <- vDeclare "0" (ValInt i) m
  return m
