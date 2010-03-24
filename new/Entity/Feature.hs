{-# LANGUAGE TemplateHaskell #-}
module Entity.Feature (features) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Entity

-- Given an optional type for custom data and some feature names, 
-- and assuming a function called 'update' is defined,
-- this function generates the boilerplate for an entity.
features :: Maybe Name -> [Name] -> Q [Dec]
features t ns = do
    fs <- mapM makeField ns
    let fs' = case t of 
            Just t' -> (name "extra", NotStrict, ConT t'):fs
            Nothing -> fs
    let d = DataD [] (name "Data") [] [RecC (name "Data") fs'] []
    is <- mapM makeInstance ns
    let bs = map (makeCall "before") ns
    let as = map (makeCall "after") ns
    let u = DoE (map NoBindS bs ++ [NoBindS (VarE (name "update"))] ++ map NoBindS as)
    let u' = InstanceD [] (AppT (ConT ''Entity.Class) (ConT (name "Data"))) [
            FunD (name "updateEntity") [Clause [] (NormalB (AppE (VarE 'Entity.monadicUpdate) u)) []]]
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

name s = Name (mkOccName s) NameS

