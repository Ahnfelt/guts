module PlayerEntity (playerNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import FlameEntity
import PelletEntity
import Damage
import GameState
import KeyState
import Mechanics
import Message

data Player = Player { 
    playerPosition :: Position,
    playerAimAngle :: Angle,
    playerMoveAngle :: Angle,
    playerHealth :: Double,
    playerKeys :: (KeyButton, KeyButton, KeyButton, KeyButton, KeyButton, KeyButton),
    playerShotgunInterval :: Interval,
    playerFlameInterval :: Interval,
    playerId :: Unique
} deriving Show

-- (position, (up, down, left, right, primary, secondary))
playerNew :: Position -> (KeyButton, KeyButton, KeyButton, KeyButton, KeyButton, KeyButton) -> (Unique -> AbstractEntity)
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

instance Entity Player where

    entityUpdate e s m r d | playerHealth e <= 0 = deltaStateNew
    entityUpdate e s m r d =
        let damage = sum $ map (\m -> case m of
                MessageDamage e' damage -> damageHealth (damageResistanceNew) damage
                _ -> 0.0) m in
        let r1:r2:rs = randoms (mkStdGen r) in
        let k = stateKeys s in
        let (keyUp, keyDown, keyLeft, keyRight, keyPrimary, keySecondary) = playerKeys e in
        let p = playerPosition e in
        let a = playerMoveAngle e in
        let (a', k') = case (k keyUp, k keyDown, k keyLeft, k keyRight) of
                (True, _, True, _) -> (-0.75 * pi, True)
                (True, _, _, True) -> (-0.25 * pi, True)
                (_, True, True, _) -> (-1.25 * pi, True)
                (_, True, _, True) -> (-1.75 * pi, True)
                (True, _, _, _) -> (-0.50 * pi, True)
                (_, True, _, _) -> (-1.50 * pi, True)
                (_, _, True, _) -> (-1.00 * pi, True)
                (_, _, _, True) -> (-0.00 * pi, True)
                _ -> (a, False) in
        let aa = playerAimAngle e in
        let (ad, md) = if not k' then (0, 0) else 
                if k keyPrimary || k keySecondary then (5, 80) else (10, 120) in
        let p' = p .+ velocity a' (md * d) in
        let aa' = if k keyPrimary || k keySecondary then aa else approximateAngle (ad * d) aa a in
        let (flameShots, flameInterval) = intervalsSince (playerFlameInterval e) d (k keyPrimary) in
        let es = if k keyPrimary then take flameShots $ map (fireFlame p' aa' 0.30) rs else [] in
        let (shotgunShots, shotgunInterval) = intervalsSince (playerShotgunInterval e) d (k keySecondary) in
        let es' = if k keySecondary then concat $ take shotgunShots $ map (fireShotgun p' aa' 0.20) rs else [] in
        deltaStateNew { 
            deltaEntities = const (AbstractEntity (e { 
                playerAimAngle = aa',
                playerMoveAngle = a',
                playerHealth = playerHealth e - damage,
                playerShotgunInterval = shotgunInterval,
                playerFlameInterval = flameInterval,
                playerPosition = p'})):es++es', 
            deltaSplatter = Just $ do
                when k' $ do
                    rotate (playerAimAngle e)
                    setSourceRGBA 0 0.1 0 0.08
                    arc 0 5 3 0 (2 * pi)
                    fill
                    arc 0 (-5) 3 0 (2 * pi)
                    fill
        }
    
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

    entityOnTop e = True
    
    entityHitable e = True

    entityId e = playerId e

-- (start, angle, spread, seed)
fireFlame :: Position -> Angle -> Angle -> Int -> (Unique -> AbstractEntity)
fireFlame p a s r =
    let (r0, r') = random (mkStdGen r) in
    let r1:r2:r3:_ = randoms r' in 
    flameNew 
        (p .+ velocity a (20 + 5 * r2))
        (velocity (a - 0.5 * s + r1 * s) (100 + r2 * 50) .* 1.5)
        a
        1.0
        r0

fireShotgun :: Position -> Angle -> Angle -> Int -> [(Unique -> AbstractEntity)]
fireShotgun position angle spread seed =
    let (seed', generator) = random (mkStdGen seed) in
    take 10 $ map (firePellet seed') (randoms generator)
    where
        firePellet seed multiplier =
            let angle' = angle - 0.5 * spread + multiplier * spread in 
            pelletNew 
                (position .+ velocity angle' 20)
                (velocity angle' 600)
                angle'
                1.0
                seed

