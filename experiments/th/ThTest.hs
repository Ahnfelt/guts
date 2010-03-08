{-# OPTIONS_GHC -fth #-}
module Main where

import TupleReplicate
import Language.Haskell.TH


z1 = $(return $ LamE [WildP] (LitE (IntegerL 0)))
--z2 = return [| \_ -> 0 |]

main = do print ($(tupleReplicate 2) 1)     -- prints (1,1)
          print ($(tupleReplicate 5) "x")   -- prints ("x","x","x","x","x")
          print ($(summ 3) 1 2 3)
          print ($([| $(summ 2) 20 22 |]))


