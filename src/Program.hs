module Program where

import Data.Map

import Parsing.AbsLatte
import Parsing.LexLatte   ( Token )
import Parsing.SkelLatte ( Err, Result )
import TopDef
import qualified TopScope

trans :: Parsing.AbsLatte.Program () -> Result
trans (Program () l) = startProgram (TopScope.from l)

getMain :: [TopDef ()] -> Err (TopDef ())
getMain (h:t) = case h of
  (FnDef () (Int ()) (Ident "main") [] _) -> Right h
  h -> getMain t
getMain [] = Left "Program: main() not declared"

startProgram :: Map String Int -> Result
startProgram m = Right $ show $ Data.Map.lookup "main" m