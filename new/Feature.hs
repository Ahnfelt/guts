{-# LANGUAGE TemplateHaskell #-}
module Feature (features) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Entity

name s = Name (mkOccName s) NameS

features :: [Name] -> Q [Dec]
features ns = do
    let f = (name "extra", NotStrict, ConT (name "Extra"))
    fs <- mapM makeField ns
    let d = DataD [] (name "Data") [] [RecC (name "Data") (f:fs)] []
    is <- mapM makeInstance ns
    let bs = map (makeCall "before") ns
    let as = map (makeCall "after") ns
    let u = DoE (map NoBindS bs ++ [NoBindS (VarE (name "update"))] ++ map NoBindS as)
    let u' = InstanceD [] (AppT (ConT ''Entity.Class) (ConT (name "Data"))) [
            FunD (name "updateEntity") [Clause [] (NormalB u) []]]
    return (u':d:is)

makeField n = do
    let Just m = nameModule n
    return ((name ("data" ++ m)), NotStrict, ConT (Name (mkOccName "Data") (NameQ (mkModName m))))

makeInstance n = do
    let Just m = nameModule n
    let g = name ("data" ++ m)
    e <- [| \f -> Entity.modify $ \e -> do 
                d <- f ($(varE g) e)
                return ($(recUpdE [|e|] [do d' <- [|d|]; return (g, d')])) 
            |]
    let d = FunD (name "modify") [Clause [] (NormalB e) []]
    return $ InstanceD [] (AppT (ConT n) (ConT (name "Data"))) [d]

makeCall f n =
    let Just m = nameModule n in
    let f' = Name (mkOccName f) (NameQ (mkModName m)) in
    VarE f'

