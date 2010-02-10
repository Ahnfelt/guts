module PlayerEntity (playerNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import FlameEntity
import PelletEntity
import Layer
import Damage
import GameState
import KeyState
import Mechanics
import Message
import Tile

data Player = Player { 
    playerPosition :: Position,
    playerAimAngle :: Angle,
    playerMoveAngle :: Angle,
    playerHealth :: Double,
    playerKeys :: [KeyButton],
    playerShotgunInterval :: Interval,
    playerFlameInterval :: Interval,
    playerId :: Unique
} deriving Show

-- (position, [up, down, left, right, primary, secondary])
playerNew :: Position -> [KeyButton] -> (Unique -> AbstractEntity)
playerNew p k = \i -> AbstractEntity $ Player {
    playerPosition = p,
    playerAimAngle = 0,
    playerMoveAngle = 0,
    playerHealth = 1.0,
    playerKeys = k,
    playerShotgunInterval = intervalNew 0.80,
    playerFlameInterval = intervalNew 0.03,
    playerId = i
    }

instance EntityAny Player

instance Entity Player where

    entityUpdate = entityUpdater $ \d -> do
        receive $ do
            MessageDamage d <- message
            let d' = damageHealth damageResistanceNew d
            change $ \e -> e { playerHealth = playerHealth e - d' }
        e <- self
        [keyUp, keyDown, keyLeft, keyRight, keyPrimary, keySecondary] <- mapM pressed (playerKeys e)
        let keyDirectional = keyUp || keyDown || keyLeft || keyRight
        let newMoveAngle = case (keyUp, keyDown, keyLeft, keyRight) of
                (True, _, True, _) -> -0.75 * pi
                (True, _, _, True) -> -0.25 * pi
                (_, True, True, _) -> -1.25 * pi
                (_, True, _, True) -> -1.75 * pi
                (True, _, _, _) -> -0.50 * pi
                (_, True, _, _) -> -1.50 * pi
                (_, _, True, _) -> -1.00 * pi
                (_, _, _, True) -> -0.00 * pi
                _ -> playerMoveAngle e
        when keyDirectional $ splatter $ do
            rotate (playerAimAngle e)
            setSourceRGBA 0 0.1 0 0.08
            arc 0 5 3 0 (2 * pi)
            fill
            arc 0 (-5) 3 0 (2 * pi)
            fill
        let (turnSpeed, movementSpeed) = 
                if not keyDirectional then (0, 0)
                else if keyPrimary || keySecondary then (8, 80) 
                else (12, 120)
        let newPosition = playerPosition e .+ velocity newMoveAngle (movementSpeed * d)
        let newAimAngle = approximateAngle (turnSpeed * d) (playerAimAngle e) newMoveAngle
        let (flameShots, flameInterval) = intervalsSince (playerFlameInterval e) d keyPrimary
        when keyPrimary $ replicateM_ flameShots $ do
            fireFlame newPosition newAimAngle 0.30
        let (shotgunShots, shotgunInterval) = intervalsSince (playerShotgunInterval e) d keySecondary
        when keySecondary $ replicateM_ shotgunShots $ do
            fireShotgun newPosition newAimAngle 0.20
        change $ \e -> e { 
            playerAimAngle = newAimAngle,
            playerMoveAngle = newMoveAngle,
            playerFlameInterval = flameInterval,
            playerShotgunInterval = shotgunInterval,
            playerPosition = newPosition
        }
        e <- self
        when (playerHealth e <= 0) $ do
            vanish

    entityPosition e = Just (playerPosition e)

    entityRadius e = Just 10

    entityDraw e i = do
        rotate (playerAimAngle e)
        setSourceRGB 1 0 0
        arc 0 0 10 0 (2 * pi)
        fill
        setSourceRGB 0.5 0 0
        arc 0 0 10 (-0.5) (0.5)
        lineTo 0 0
        fill

    entityLayer e = LayerPlayer
    
    entityHitable e = True

    entityId e = playerId e

fireFlame :: (EntityAny e) => Position -> Angle -> Angle -> EntityMonad k e ()
fireFlame position angle spread = do
    positionRandom <- randomDouble
    spreadRandom <- randomDouble
    velocityRandom <- randomDouble
    spawn $ flameNew 
        (position .+ velocity angle (20 + 5 * positionRandom))
        (velocity (angle - 0.5 * spread + spreadRandom * spread) (100 + velocityRandom * 50) .* 1.5)
        angle
        1.0
        42

fireShotgun :: (EntityAny e) => Position -> Angle -> Angle -> EntityMonad k e ()
fireShotgun position angle spread = replicateM_ 40 $ do
    spreadRandom <- randomDouble
    speedRandom <- randomDouble
    timeToLiveRandom <- randomDouble
    let spreadRandom' = 1 - (log(1 / spreadRandom - 1) + 6) / 12
    let angle' = angle - 0.5 * spread + spreadRandom' * spread
    spawn $ pelletNew 
        (position .+ velocity angle' 20)
        (velocity angle' (600 + speedRandom * 150))
        angle'
        (0.3 + timeToLiveRandom * 0.3)

