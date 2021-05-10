-- Haskell module generated by the BNF converter

module Parsing.SkelLatte where

import qualified Parsing.AbsLatte

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Parsing.AbsLatte.Ident -> Result
transIdent x = case x of
  Parsing.AbsLatte.Ident string -> failure x
transProgram :: Show a => Parsing.AbsLatte.Program a -> Result
transProgram x = case x of
  Parsing.AbsLatte.Program _ topdefs -> failure x
transTopDef :: Show a => Parsing.AbsLatte.TopDef a -> Result
transTopDef x = case x of
  Parsing.AbsLatte.FnDef _ type_ ident args block -> failure x
transArg :: Show a => Parsing.AbsLatte.Arg a -> Result
transArg x = case x of
  Parsing.AbsLatte.VArg _ type_ ident -> failure x
  Parsing.AbsLatte.RArg _ type_ ident -> failure x
transBlock :: Show a => Parsing.AbsLatte.Block a -> Result
transBlock x = case x of
  Parsing.AbsLatte.Block _ stmts -> failure x
transStmt :: Show a => Parsing.AbsLatte.Stmt a -> Result
transStmt x = case x of
  Parsing.AbsLatte.Empty _ -> failure x
  Parsing.AbsLatte.BStmt _ block -> failure x
  Parsing.AbsLatte.Decl _ type_ items -> failure x
  Parsing.AbsLatte.Ass _ ident expr -> failure x
  Parsing.AbsLatte.Incr _ ident -> failure x
  Parsing.AbsLatte.Decr _ ident -> failure x
  Parsing.AbsLatte.Ret _ expr -> failure x
  Parsing.AbsLatte.VRet _ -> failure x
  Parsing.AbsLatte.Cond _ expr stmt -> failure x
  Parsing.AbsLatte.CondElse _ expr stmt1 stmt2 -> failure x
  Parsing.AbsLatte.While _ expr stmt -> failure x
  Parsing.AbsLatte.SExp _ expr -> failure x
transItem :: Show a => Parsing.AbsLatte.Item a -> Result
transItem x = case x of
  Parsing.AbsLatte.NoInit _ ident -> failure x
  Parsing.AbsLatte.Init _ ident expr -> failure x
transType :: Show a => Parsing.AbsLatte.Type a -> Result
transType x = case x of
  Parsing.AbsLatte.Int _ -> failure x
  Parsing.AbsLatte.Str _ -> failure x
  Parsing.AbsLatte.Bool _ -> failure x
  Parsing.AbsLatte.Void _ -> failure x
  Parsing.AbsLatte.Fun _ type_ types -> failure x
transExpr :: Show a => Parsing.AbsLatte.Expr a -> Result
transExpr x = case x of
  Parsing.AbsLatte.EVar _ ident -> failure x
  Parsing.AbsLatte.ELitInt _ integer -> failure x
  Parsing.AbsLatte.ELitTrue _ -> failure x
  Parsing.AbsLatte.ELitFalse _ -> failure x
  Parsing.AbsLatte.EApp _ ident exprs -> failure x
  Parsing.AbsLatte.EString _ string -> failure x
  Parsing.AbsLatte.Neg _ expr -> failure x
  Parsing.AbsLatte.Not _ expr -> failure x
  Parsing.AbsLatte.EMul _ expr1 mulop expr2 -> failure x
  Parsing.AbsLatte.EAdd _ expr1 addop expr2 -> failure x
  Parsing.AbsLatte.ERel _ expr1 relop expr2 -> failure x
  Parsing.AbsLatte.EAnd _ expr1 expr2 -> failure x
  Parsing.AbsLatte.EOr _ expr1 expr2 -> failure x
transAddOp :: Show a => Parsing.AbsLatte.AddOp a -> Result
transAddOp x = case x of
  Parsing.AbsLatte.Plus _ -> failure x
  Parsing.AbsLatte.Minus _ -> failure x
transMulOp :: Show a => Parsing.AbsLatte.MulOp a -> Result
transMulOp x = case x of
  Parsing.AbsLatte.Times _ -> failure x
  Parsing.AbsLatte.Div _ -> failure x
  Parsing.AbsLatte.Mod _ -> failure x
transRelOp :: Show a => Parsing.AbsLatte.RelOp a -> Result
transRelOp x = case x of
  Parsing.AbsLatte.LTH _ -> failure x
  Parsing.AbsLatte.LE _ -> failure x
  Parsing.AbsLatte.GTH _ -> failure x
  Parsing.AbsLatte.GE _ -> failure x
  Parsing.AbsLatte.EQU _ -> failure x
  Parsing.AbsLatte.NE _ -> failure x

