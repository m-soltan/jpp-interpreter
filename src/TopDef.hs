module TopDef where

import Parsing.AbsLatte
import Parsing.SkelLatte (Err, Result)

trans :: Parsing.AbsLatte.TopDef () -> Result
trans (FnDef _ type_ ident args block) = Left "not implemented"
