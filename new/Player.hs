{-# LANGUAGE TemplateHaskell #-}
module Player where
import qualified Collidable
import Feature
import Entity

data Extra = Extra {
}

$(features [''Collidable.Class])

update = do 
    return ()

