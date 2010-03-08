{-# OPTIONS_GHC -fth #-}
module TupleReplicate where
 
import Language.Haskell.TH
 
tupleReplicate :: Int -> Q Exp
tupleReplicate n = do id <- newName "x"
                      return $ LamE [(VarP id)]
                                    (TupE $ replicate n $ VarE id)

summ n = summ' n [| 0 |]
summ' 0 code = code
summ' n code = [| \x -> $(summ' (n-1) [|$code+x|] ) |]

