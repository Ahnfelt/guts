module PelletEntity (pelletNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import Damage
import GameState
import Mechanics
import Message

data Pellet = Pellet { 
    pelletPosition :: Position,
    pelletVelocity :: Velocity,
    pelletTime :: Duration,
    pelletTimeLeft :: Duration,
    pelletAngle :: Angle,
    pelletId :: Unique
} deriving Show

-- (position, velocity, angle, timeToLive, seed)
pelletNew :: Position -> Velocity -> Angle -> Duration -> Int -> (Unique -> AbstractEntity)
pelletNew p v a t r = {-let r1:_ = randoms (mkStdGen r) in-} \i -> AbstractEntity $ Pellet {
    pelletPosition = p,
    pelletVelocity = v,
    pelletTime = t,
    pelletTimeLeft = t,
    pelletAngle = a,
    pelletId = i}

instance Entity Pellet where

    entityUpdate e s m r d | pelletTimeLeft e <= 0 = deltaStateNew
    entityUpdate e s m r d =
        let m' = concat $ map (\m -> case m of
                MessageCollide e' -> [(entityId e', MessageDamage (AbstractEntity e) (damageNew { damagePiercing = 0.02 }))]
                _ -> []) m in
        let (x', y') = pelletPosition e .+ (pelletVelocity e .* d) in
        let newEntities = if not $ null m' then [] else [const (AbstractEntity (e { 
                pelletPosition = (x', y'),
                pelletTimeLeft = pelletTimeLeft e - d
                }))] in 
        deltaStateNew {
            deltaEntities = newEntities,
            deltaMessages = m'
        }
    
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

    entityOnTop e = True

    entityHitable e = False

    entityId e = pelletId e

-- Calculates the age as a fraction of the total lifetime (0: youngest, 1: oldest)
age e = 1 - pelletTimeLeft e / pelletTime e

