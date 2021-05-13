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
      case except m of
        Right _ -> do
          m <- transStmt h m
          transBlock (Block a t) m
        Left err -> return m

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
transStmt (Cond _ e s) m = do
  m <- transExpr e m
  case vRead "0" m of
    Just (ValBool True) -> do
      m <- transStmt s m
      return m
    Just (ValBool False) -> do
      return m
    Just _ -> do
      let m1 = addError "non-boolean condition in if statement" m
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
transExpr (EVar _ _) m = do
  let m1 = addError "variable expressions not implemented" m
  return m1
transExpr (ELitInt _ i) m = do
  m <- vDeclare "0" (ValInt i) m
  return m
transExpr (ELitTrue _) m = do
  let m1 = addError "\"true\" literal not implemented" m
  return m1
transExpr (ELitFalse _) m = do
  let m1 = addError "\"false\" literal not implemented" m
  return m1
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
transExpr (EString _ _) m = do
  let m1 = addError "String literals not implemented" m
  return m1
transExpr (Neg _ _) m = do
  let m1 = addError "arithmetic negation in expressions not implemented" m
  return m1
transExpr (Not _ _) m = do
  let m1 = addError "logical negation in expressions not implemented" m
  return m1
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
