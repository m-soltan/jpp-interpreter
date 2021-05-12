module Src.TopDef where

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte (Err, Result)

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
transStmt (Ret _ e) m = do
  retVal <- transExpr e m
  m <- transExpr e m
  return m
transStmt (SExp _ e) m = do
  m <- transExpr e m
  return m

transExpr :: Expr a -> MemoryState a -> IO (MemoryState a)
transExpr (EApp _ (Ident "printString") l) m = do
  case l of
    [EString _ str] -> do
      putStrLn str
      return m
transExpr (EApp _ (Ident ident) l) m = do
  let f = getFunction ident m
  callFunc f m
transExpr (ELitInt _ i) m = do
  m <- vDeclare "0" (ValInt i) m
  return m
