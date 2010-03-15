module Entity.Player (playerNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import Layer
import GameState
import World.Mechanics
import Message
import Message.Damage
import World.Tile
import Entity.Weapon
import Entity.Weapon.Shotgun
import Entity.Weapon.Flamethrower
import Entity.Projectile.Flame
import Entity.Projectile.Pellet


data Player = Player { 
    playerAimAngle :: Angle,
    playerMoveAngle :: Angle,
    playerHealth :: Double,
    playerKeys :: [KeyButton],
    playerShotgunInterval :: Interval,
    playerFlameInterval :: Interval,
    playerActor :: Actor,
    playerWeapons :: [AbstractWeapon]
} deriving Show


playerNew :: Position -> [KeyButton] -> (Unique -> AbstractEntity)
playerNew p k =
    let (ds, [shotgunInterval, flameInterval]) = actorIntervalsNew [0.080, 0.03] in
    \u -> AbstractEntity $ Player {
        playerAimAngle = 0,
        playerMoveAngle = 0,
        playerHealth = 1.0,
        playerKeys = k,
        playerShotgunInterval = shotgunInterval,
        playerFlameInterval = flameInterval,
        playerActor = actorNew u p (0, 0) 99999999 ds, 
        playerWeapons = [newFlamethrower, newShotgun]
    }

instance EntityActor Player where
    actorGet e = playerActor e
    actorSet e a = e { playerActor = a }

instance EntityAny Player

instance Entity Player where

    entityUpdate = actorUpdater $ do
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
        actorMoveTowards (velocity newMoveAngle movementSpeed)
        d <- timePassed
        e <- self
        let newAimAngle = approximateAngle (turnSpeed * d) (playerAimAngle e) newMoveAngle
        let weapons = (playerWeapons e)
        let newWeapons = if keySecondary then tail weapons ++ [head weapons] else weapons
        let selectedWeapon = head newWeapons
        when keyPrimary $ fire selectedWeapon (actorPosition $ playerActor e) newAimAngle
        change $ \e -> e { 
            playerAimAngle = newAimAngle,
            playerMoveAngle = newMoveAngle,
            playerWeapons = newWeapons
        }
        e <- self
        when (playerHealth e <= 0) $ do
            vanish

    entityPosition e = Just (actorPosition $ playerActor e)

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

    entityId e = actorId $ playerActor e

