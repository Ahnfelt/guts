{-# LANGUAGE TemplateHaskell #-}
module Entity.Player (playerNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import World.Mechanics
import Message
import Message.Damage
import World.Tile
import Entity
import Entity.Feature


data Extra = Extra { 
}

$(features [])

playerNew :: Position -> [String] -> (Unique -> AbstractEntity)
playerNew p k =
    \u -> AbstractEntity $ Data {
    }

-- TODO: Convert this to M e ()
update e = DeltaState {}
    

