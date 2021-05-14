module Src.TopDef where

import Data.Map

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte (Err, Result)
import Src.Util

transTopDef :: Src.Parsing.AbsLatte.TopDef a -> MemoryState a -> IO Result
transTopDef (FnDef _ type_ ident args block) m = do
  return (Left "not implemented")

callFunc :: Src.Parsing.AbsLatte.TopDef a -> MemoryState a -> IO (MemoryState a)
callFunc f m = case f of
  FnDef _ t (Ident k) args block -> do
    let m1 = vDeclare
    callResult <- transBlock block (functionScope m)
    case except callResult of
      Right "ok" -> case ans callResult of
        Just v -> do
          m <- vHold v m
          return m
        Nothing -> do
          let m1 = addError "function ended with no return statement" m
          return m1
      Left err -> do
        return (addError err m)

blockAux :: [Stmt a] -> MemoryState a -> IO (MemoryState a)
blockAux [] m = do
  return m
blockAux (h : t) m = do
  m <- transStmt h m
  case except m of
    Right "ok" -> blockAux t m
    Left _ -> return m

transBlock :: Src.Parsing.AbsLatte.Block a -> MemoryState a -> IO (MemoryState a)
transBlock (Block a l) m = do
  let local = vLocal m
  let m1 = setLocal empty m
  m1 <- blockAux l m1
  let m2 = setLocal local m1
  return m2

transItems :: Type a -> [Src.Parsing.AbsLatte.Item a] -> MemoryState a -> IO (MemoryState a)
transItems _ [] m = do
  return m
transItems tp (h : t) m = case h of
  NoInit a (Ident ident) -> do
    m <- vDeclare ident tp m
    return m
  Init _ ident e -> do
    m <- transExpr e m
    case except m of
      Right "ok" -> do
        case ident of
          Ident str -> do
            let v = vUnhold m
            m <- vInitialize str v m
            m <- transItems tp t m
            return m
      _ -> return m

transStmt :: Stmt a -> MemoryState a -> IO (MemoryState a)
transStmt (Empty _) m = do
  return m
transStmt (BStmt _ b) m = do
  m <- transBlock b m
  return m
transStmt (Decl _ t l) m = do
  m <- transItems t l m
  return m
transStmt (Ass _ (Ident ident) e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> do
      let newValue = vUnhold m
      case (vGetType ident m, newValue) of
        (Just (Str _), ValStr _ _) -> return (vReplace ident newValue m)
        (Just (Int _), ValInt _ _) -> return (vReplace ident newValue m)
        (Just (Bool _), ValBool _ _) -> return (vReplace ident newValue m)
        _ -> do
          let m1 = addError "assignment on unsupported types" m
          return m1
    Left _ -> return m
transStmt (Incr _ ident) m = do
  case ident of
    Ident str -> do
      let v = vRead str m
      case v of
        Just (ValInt a i) -> do
          let newValue = ValInt a (i + 1)
          let m1 = vReplace str newValue m
          return m1
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
        Just (ValInt a i) -> do
          let newValue = ValInt a (i - 1)
          let m1 = vReplace str newValue m
          return m1
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
transStmt (VRet a) m = do
  m <- vHold (ValVoid a) m
  let m1 = addRetVal m
  return m1
transStmt (Cond _ e s) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vUnhold m of
      ValBool _ True -> do
        m <- transStmt s m
        return m
      ValBool _ False -> do
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
      ValBool _ True -> do
        m <- transStmt l m
        return m
      ValBool _ False -> do
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
      ValBool _ True -> do
        m <- transStmt s m
        m <- transStmt (While a e s) m
        return m
      ValBool _ False -> do
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
transExpr (ELitInt a i) m = do
  m <- vHold (ValInt a i) m
  return m
transExpr (ELitTrue a) m = do
  m <- vHold (ValBool a True) m
  return m
transExpr (ELitFalse a) m = do
  m <- vHold (ValBool a False) m
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
transExpr (EString a str) m = do
  m <- vHold (ValStr a str) m
  return m
transExpr (Neg _ e) m = do
  m <- transExpr e m
  case except m of
    Right "ok" -> case vUnhold m of
      ValInt a i -> do
        let newValue = ValInt a (-i)
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
      ValBool a b -> do
        let newValue = ValBool a (not b)
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
            (ValInt a li, ValInt _ ri) -> do
              let newValue = ValInt a (li * ri)
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
            (ValInt a li, ValInt _ ri) -> do
              let newValue = ValInt a (li + ri)
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
            (ValBool a lb, ValBool _ rb) -> do
              let newValue = ValBool a (lb == rb)
              m <- vHold newValue m
              return m
            (ValInt a li, ValInt _ ri) -> do
              let newValue = ValBool a (li == ri)
              m <- vHold newValue m
              return m
            (ValStr a ls, ValStr _ rs) -> do
              let newValue = ValBool a (ls == rs)
              m <- vHold newValue m
              return m
            (ValBool a _, ValInt _ _) -> do
              let m1 = addError "test" m
              return m1
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
            (ValBool a lb, ValBool _ rb) -> do
              let newValue = ValBool a (lb && rb)
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
            (ValBool a lb, ValBool _ rb) -> do
              let newValue = ValBool a (lb || rb)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to ||" m
              return m1
        Left _ -> return m
    Left _ -> return m
