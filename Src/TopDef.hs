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
          m <- vHold v m
          return m
        Nothing -> do
          let m1 = addError "function ended with no return statement" m
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

transItems :: [Src.Parsing.AbsLatte.Item a] -> MemoryState a -> IO (MemoryState a)
transItems [] m = do
  return m
transItems (h : t) m = case h of
  NoInit _ ident -> case ident of
    Ident str -> vDeclare str ValVoid m
  Init _ ident e -> do
    m <- transExpr e m
    case except m of
      Right "ok" -> do
        case ident of
          Ident str -> do
            let v = vUnhold m
            m <- vDeclare str v m
            return m
      _ -> return m

transStmt :: Stmt a -> MemoryState a -> IO (MemoryState a)
transStmt (Empty _) m = do
  return m
transStmt (BStmt _ b) m = do
  let m1 = addError "block statement not implemented" m
  return m1
transStmt (Decl _ t l) m = do
  m <- transItems l m
  return m
transStmt (Ass _ ident e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> do
      case ident of
        Ident str -> do
          let v = vUnhold m
          m <- vDeclare str v m
          return m
transStmt (Incr _ ident) m = do
  case ident of
    Ident str -> do
      let v = vRead str m
      case v of
        Just (ValInt i) -> do
          let newValue = ValInt (i + 1)
          m <- vDeclare str newValue m
          return m
        Just _ -> do
          let m1 = addError "attempt to increment a non-integer" m
          return m1
        Nothing -> do
          let m1 = addError "increment on an undeclared identifier" m
          return m1
transStmt (Decr _ ident) m = do
  case ident of
    Ident str -> do
      let v = vRead str m
      case v of
        Just (ValInt i) -> do
          let newValue = ValInt (i - 1)
          m <- vDeclare str newValue m
          return m
        Just _ -> do
          let m1 = addError "attempt to decrement a non-integer" m
          return m1
        Nothing -> do
          let m1 = addError "decrement on an undeclared identifier" m
          return m1
transStmt (Ret _ e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> do
      let m1 = addRetVal m
      return m1
    Left _ -> return m
transStmt (VRet _) m = do
  m <- vHold ValVoid m
  let m1 = addRetVal m
  return m1
transStmt (Cond _ e s) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vUnhold m of
      ValBool True -> do
        m <- transStmt s m
        return m
      ValBool False -> do
        return m
      _ -> do
        let m1 = addError "non-boolean condition in if statement" m
        return m1
    Left err -> do
      return m
transStmt (CondElse _ e l r) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vUnhold m of
      ValBool True -> do
        m <- transStmt l m
        return m
      ValBool False -> do
        m <- transStmt r m
        return m
      _ -> do
        let m1 = addError "non-boolean condition in if-else statement" m
        return m1
    Left err -> do
      return m
transStmt (While a e s) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vUnhold m of
      ValBool True -> do
        m <- transStmt s m
        m <- transStmt (While a e s) m
        return m
      ValBool False -> do
        return m
    Left _ -> return m
transStmt (SExp _ e) m = do
  m <- transExpr e m
  return m


transExpr :: Expr a -> MemoryState a -> IO (MemoryState a)
transExpr (EVar _ ident) m = case ident of
  Ident str -> do
    case vRead str m of
      Just v -> do
        m <- vHold v m
        return m
      Nothing -> do
        let m1 = addError "expression is an undeclared identifier" m
        return m1
transExpr (ELitInt _ i) m = do
  m <- vHold (ValInt i) m
  return m
transExpr (ELitTrue _) m = do
  m <- vHold (ValBool True) m
  return m
transExpr (ELitFalse _) m = do
  m <- vHold (ValBool False) m
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
  m <- vHold (ValStr str) m
  return m
transExpr (Neg _ e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vUnhold m of
      ValInt i -> do
        let newValue = ValInt (-i)
        m <- vHold newValue m
        return m
      _ -> do
        let m1 = addError "invalid type passed to arithmetic negation" m
        return m1
    Left err -> return m
transExpr (Not _ e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vUnhold m of
      ValBool b -> do
        let newValue = ValBool (not b)
        m <- vHold newValue m
        return m
      _ -> do
        let m1 = addError "invalid type passed to logical negation" m
        return m1
    Left err -> return m
transExpr (EMul _ l _ r) m = do
  m <- transExpr l m
  case except m of
    Right "ok" -> do
      let left = vUnhold m
      m <- transExpr r m
      case except m of
        Right "ok" -> do
          let right = vUnhold m
          case (left, right) of
            (ValInt li, ValInt ri) -> do
              let newValue = ValInt (li * ri)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to arithmetic multiplication" m
              return m1
        Left _ -> return m
    Left _ -> return m
transExpr (EAdd _ l _ r) m = do
  m <- transExpr l m
  case except m of
    Right "ok" -> do
      let left = vUnhold m
      m <- transExpr r m
      case except m of
        Right "ok" -> do
          let right = vUnhold m
          case (left, right) of
            (ValInt li, ValInt ri) -> do
              let newValue = ValInt (li + ri)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to arithmetic addition" m
              return m1
        Left _ -> return m
    Left _ -> return m
transExpr (ERel _ l _ r) m = do
  m <- transExpr l m
  case except m of
    Right "ok" -> do
      let left = vUnhold m
      m <- transExpr r m
      case except m of
        Right "ok" -> do
          let right = vUnhold m
          case (left, right) of
            (ValBool lb, ValBool rb) -> do
              let newValue = ValBool (lb == rb)
              m <- vHold newValue m
              return m
            (ValInt li, ValInt ri) -> do
              let newValue = ValBool (li == ri)
              m <- vHold newValue m
              return m
            (ValStr ls, ValStr rs) -> do
              let newValue = ValBool (ls == rs)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to comparison" m
              return m1
        Left _ -> return m
    Left _ -> return m
transExpr (EAnd _ l r) m = do
  m <- transExpr l m
  case except m of
    Right "ok" -> do
      let left = vUnhold m
      m <- transExpr r m
      case except m of
        Right "ok" -> do
          let right = vUnhold m
          case (left, right) of
            (ValBool lb, ValBool rb) -> do
              let newValue = ValBool (lb && rb)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to &&" m
              return m1
        Left _ -> return m
    Left _ -> return m
transExpr (EOr _ l r) m = do
  m <- transExpr l m
  case except m of
    Right "ok" -> do
      let left = vUnhold m
      m <- transExpr r m
      case except m of
        Right "ok" -> do
          let right = vUnhold m
          case (left, right) of
            (ValBool lb, ValBool rb) -> do
              let newValue = ValBool (lb || rb)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to ||" m
              return m1
        Left _ -> return m
    Left _ -> return m
