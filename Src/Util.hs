module Src.Util where

(|>) x f = f x

dbgPrint :: String -> IO ()
-- dbgPrint = (\_ -> return ())
dbgPrint = putStrLn
