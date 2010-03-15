{-# LANGUAGE TemplateHaskell #-}
module Entity.Monster.Panda where
import qualified Collidable
import Entity
import Entity.Feature

data Extra = Extra {
}

$(features [''Collidable.Class])

update e = do 
    return ()

