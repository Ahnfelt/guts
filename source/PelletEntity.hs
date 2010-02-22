module PelletEntity (pelletNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import Layer
import Damage
import GameState
import Mechanics
import Message
import Tile

data Pellet = Pellet { 
    pelletPosition :: Position,
    pelletVelocity :: Velocity,
    pelletTime :: Duration,
    pelletTimeLeft :: Duration,
    pelletAngle :: Angle,
    pelletId :: Unique
} deriving Show

-- (position, velocity, angle, timeToLive, seed)
pelletNew :: Position -> Velocity -> Angle -> Duration -> (Unique -> AbstractEntity)
pelletNew p v a t = \i -> AbstractEntity $ Pellet {
    pelletPosition = p,
    pelletVelocity = v,
    pelletTime = t,
    pelletTimeLeft = t,
    pelletAngle = a,
    pelletId = i}

instance EntityAny Pellet

instance Entity Pellet where

    entityUpdate = entityUpdater $ \d -> do
        receive $ do
            MessageCollide <- message
            reply $ MessageDamage $ damageNew { damagePiercing = 0.02 }
            vanish
        e <- self
        let position = pelletPosition e
        let position' = position .+ (pelletVelocity e .* d)
        change $ \e -> e {
            pelletPosition = position',
            pelletTimeLeft = pelletTimeLeft e - d
        }
        e <- self
        when (pelletTimeLeft e <= 0) $ do
            vanish
    
    entityPosition e = Just (pelletPosition e)

    entityRadius e = Just 1

    entityDraw e i = do
        rotate (pelletAngle e)
        setLineWidth 2
        setSourceRGB 0.90 0.80 0.4 
        lineTo 8 0
        stroke
        moveTo 4 2
        setSourceRGB 0.9 0.9 0.7
        lineTo 7 0
        stroke

    entityLayer e = LayerProjectile

    entityHitable e = False

    entityId e = pelletId e

-- Calculates the age as a fraction of the total lifetime (0: youngest, 1: oldest)
age e = 1 - pelletTimeLeft e / pelletTime e

