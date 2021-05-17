module Src.TopDef where

import Data.Map

import Src.Memory
import Src.Parsing.AbsLatte
import Src.Parsing.SkelLatte (Err, Result)
import Src.Util

transTopDef :: Src.Parsing.AbsLatte.TopDef a -> MemoryState a -> IO Result
transTopDef (FnDef _ type_ ident args block) m = do
  return (Left "not implemented")

functionScope :: [Expr a] -> [Arg a] -> MemoryState a -> IO (MemoryState a)
functionScope argExprs argDecls m = case except m of
  Right "ok" -> declareArgs argExprs argDecls m

storeSingle :: Arg a -> Expr a -> MemoryState a -> MemoryState a -> IO (MemoryState a)
storeSingle arg e m fScope = do
  m <- transExpr e m
  case except m of
    Right "ok" -> do
      let newValue = vUnhold m
      case arg of
        RArg _ t (Ident ident) -> case (newValue, t) of
          (ValStr _ _, Str _) -> return (vReplace ident newValue fScope)
          (ValInt _ _, Int _) -> return (vReplace ident newValue fScope)
          (ValBool _ _, Bool _) -> return (vReplace ident newValue fScope)
          _ -> do
            let m1 = addError "assignment on unsupported types" m
            return m1
        VArg _ t (Ident ident) -> case (newValue, t) of
          (ValStr _ _, Str _) -> return (vReplace ident newValue fScope)
          (ValInt _ _, Int _) -> return (vReplace ident newValue fScope)
          (ValBool _ _, Bool _) -> return (vReplace ident newValue fScope)
          _ -> do
            let m1 = addError "assignment on unsupported types" m
            return m1
    Left _ -> return m

addArgumentValues :: [Arg a] -> [Expr a] -> MemoryState a -> IO (MemoryState a)
addArgumentValues [] [] m = do
  return m
addArgumentValues (lh : lt) (rh : rt) m = do
  m <- transExpr rh m
  case except m of
    Right "ok" -> case lh of
      RArg _ t (Ident ident) -> do
        let newIdent = "0" ++ ident
        let newValue = vUnhold m
        m <- vInitialize newIdent newValue m
        m <- addArgumentValues lt rt m
        return m
      VArg _ t (Ident ident) -> do
        let newIdent = "0" ++ ident
        let newValue = vUnhold m
        m <- vInitialize newIdent newValue m
        m <- addArgumentValues lt rt m
        return m

delArgumentValues :: [Arg a] -> MemoryState a -> IO (MemoryState a)
delArgumentValues [] m = do
  return m
delArgumentValues (h : t) m = case h of
  (VArg _ _ (Ident ident)) -> do
    m <- delArgumentValues t m
    let m1 = vDelete ("0" ++ ident) m
    return m1

copyArgsToScope :: [Arg a] -> MemoryState a -> MemoryState a -> IO (MemoryState a)
copyArgsToScope [] src dst = do
  return dst
copyArgsToScope (h : t) src dst = case h of
  RArg _ _ (Ident ident) -> do
    let newIdent = "0" ++ ident
    let newValue = vRead newIdent src
    case vRead newIdent src of
      Just v -> do
      dst <- vReplaceTyped ident v dst
      dst <- copyArgsToScope t src dst
      return dst
  VArg _ _ (Ident ident) -> do
    let newIdent = "0" ++ ident
    let newValue = vRead newIdent src
    case vRead newIdent src of
      Just v -> do
      dst <- vReplaceTyped ident v dst
      dst <- copyArgsToScope t src dst
      return dst

callFunc :: Src.Parsing.AbsLatte.TopDef a -> [Expr a] -> MemoryState a -> IO (MemoryState a)
callFunc f argExprs m = case f of
  FnDef _ t (Ident k) argDecls block -> do
    fScope <- functionScope argExprs argDecls m
    m <- addArgumentValues argDecls argExprs m
    fScope <- copyArgsToScope argDecls m fScope
    m <- delArgumentValues argDecls m
    case k of
      _ -> do
        callResult <- transBlock block fScope
        case except callResult of
          Right "ok" -> case ans callResult of
            Just v -> do
              m <- updateRefArgs argDecls callResult m
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
  case (except m, retVal m) of
    (Right "ok", Nothing) -> do
      m <- transStmt h m
      m <- blockAux t m
      return m
    _ -> return m

transBlock :: Src.Parsing.AbsLatte.Block a -> MemoryState a -> IO (MemoryState a)
transBlock (Block a l) m = do
  let m1 = blockScope m
  m1 <- blockAux l m1
  return (fromScope m1 m)

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
      case vGet ident m of
        Just (t, _) -> case (newValue, t) of
          (ValStr _ _, Str _) -> return (vReplace ident newValue m)
          (ValInt _ _, Int _) -> return (vReplace ident newValue m)
          (ValBool _ _, Bool _) -> return (vReplace ident newValue m)
          _ -> do
            let m1 = addError "assignment on unsupported types" m
            return m1
        Nothing -> do
          let m1 = addError "assignment without prior declaration" m
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
          let m1 = addError "increment on an undefined identifier" m
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
          let m1 = addError "decrement on an undefined identifier" m
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
transExpr (EApp a (Ident ident) l) m = case ident of
  "fail" -> do
    case l of
      [expr] -> do
        m <- transExpr expr m
        case vUnhold m of
          ValStr _ str -> do
            let m1 = addError str m
            return m1
          _ -> do
            let m1 = addError "invalid argument type" m
            return m1
      _ -> do
        let m1 = addError "wrong number of arguments in call to fail()" m
        return m1
  "printInt" -> do
    case l of
      [expr] -> do
        m <- transExpr expr m
        case vUnhold m of
          ValInt _ i -> do
            putStrLn (show i)
            return m
          _ -> do
            let m1 = addError "invalid argument type" m
            return m1
      _ -> do
        let m1 = addError "wrong number of arguments in call to printInt()" m
        return m1
  "printString" -> do
    case l of
      [expr] -> do
        m <- transExpr expr m
        case vUnhold m of
          ValStr _ str -> do
            putStrLn str
            return m
          _ -> do
            let m1 = addError "invalid argument type" m
            return m1
      _ -> do
        let m1 = addError "wrong number of arguments in call to printString()" m
        return m1
  "scanString" -> do
    case l of
      [] -> do
        line <- getLine
        m <- vHold (ValStr a line) m
        return m
  _ -> do
    let r = getFunction ident m
    case r of
      Just f -> do
        callFunc f l m
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
transExpr (EMul _ l op r) m = do
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
              let newValue = ValInt a ((transMulOp op) li ri)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to arithmetic multiplication" m
              return m1
        Left _ -> return m
    Left _ -> return m
transExpr (EAdd _ l op r) m = do
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
              let newValue = ValInt a ((transAddOp op) li ri)
              m <- vHold newValue m
              return m
            _ -> do
              let m1 = addError "invalid type passed to arithmetic addition" m
              return m1
        Left _ -> return m
    Left _ -> return m
transExpr (ERel _ l relOp r) m = do
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
              let newValue = ValBool a ((transRelOp relOp) lb rb)
              m <- vHold newValue m
              return m
            (ValInt a li, ValInt _ ri) -> do
              let newValue = ValBool a ((transRelOp relOp) li ri)
              m <- vHold newValue m
              return m
            (ValStr a ls, ValStr _ rs) -> do
              let newValue = ValBool a ((transRelOp relOp) ls rs)
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

transAddOp :: (Integral b) => AddOp a -> b -> b -> b
transAddOp (Plus _) = (+)
transAddOp (Minus _) = (-)

transMulOp :: (Integral b) => MulOp a -> b -> b -> b
transMulOp (Times _) = (*)
transMulOp (Div _) = div
transMulOp (Mod _) = mod

transRelOp :: (Eq b, Ord b) => RelOp a -> b -> b -> Bool
transRelOp (LTH _) = (<)
transRelOp (LE _) = (<=)
transRelOp (GTH _) = (>)
transRelOp (GE _) = (>=)
transRelOp (EQU _) = (==)
transRelOp (NE _) = (\l r -> not (l == r))
