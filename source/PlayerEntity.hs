module PlayerEntity (playerNew) where
import Graphics.Rendering.Cairo
import Control.Monad
import System.Random
import Data.Unique (Unique)
import BulletEntity
import GameState
import KeyState
import Mechanics
import Message

data Player = Player { 
    playerPosition :: Position,
    playerAimAngle :: Angle,
    playerMoveAngle :: Angle,
    playerKeys :: (KeyButton, KeyButton, KeyButton, KeyButton, KeyButton, KeyButton),
    playerId :: Unique
} deriving Show

-- (position, (up, down, left, right, primary, secondary))
playerNew :: Position -> (KeyButton, KeyButton, KeyButton, KeyButton, KeyButton, KeyButton) -> (Unique -> AbstractEntity)
playerNew p k = \i -> AbstractEntity $ Player {
    playerPosition = p,
    playerAimAngle = 0,
    playerMoveAngle = 0,
    playerKeys = k,
    playerId = i
    }

instance Entity Player where

    entityUpdate e s m r d =
        let r1:r2:_ = randoms (mkStdGen r) in
        let splatter = forM_ m $ \m -> case m of
                MessageCollide _ -> do
                    setSourceRGBA 0 0 0 0.5
                    arc 10 10 30 0 (2 * pi)
                    fill in
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
        let aa' = approximateAngle (ad * d) aa a in
        let es = if k keyPrimary then [fireBullet p' aa' 0.30 r1, fireBullet p' aa' 0.10 r2] else [] in
        DeltaState { 
            deltaEntities = const (AbstractEntity (e { 
                playerAimAngle = aa',
                playerMoveAngle = a',
                playerPosition = p'})):es, 
            deltaMessages = [],
            deltaSplatter = Just $ do
                splatter
                setSourceRGBA 0 0 0 0.03
                arc 10 10 10 0 (2 * pi)
                fill
        }
    
    entityPosition e = Just (playerPosition e)

    entityBox e = Just (20, 20)

    entityDraw e = do
        translate 10 10
        rotate (playerAimAngle e)
        setSourceRGB 1 0 0
        arc 0 0 10 0 (2 * pi)
        fill
        setSourceRGB 0.5 0 0
        arc 0 0 10 (-0.5) (0.5)
        lineTo 0 0
        fill

    entityOnTop _ = True
    
    entityHitable _ = False -- The corners of the bounding boxes are hard to work with

    entityId e = playerId e

-- (start, angle, spread, seed)
fireBullet :: Position -> Angle -> Angle -> Int -> (Unique -> AbstractEntity)
fireBullet p a s r = 
    let r1:r2:r3:r4:_ = randoms (mkStdGen r) in 
    bulletNew 
        (p .+ velocity a (20 + 5 * r4))
        (velocity (a - 0.5 * s + r1 * s) (100 + r2 * 50) .* 1.5)
        1.0

