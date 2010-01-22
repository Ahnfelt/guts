module PlayerEntity where
import Graphics.Rendering.Cairo
import Control.Monad
import BulletEntity
import GameState
import KeyState
import Mechanics
import Message

data Player = Player { 
    playerPosition :: Position,
    playerKeys :: (KeyButton, KeyButton, KeyButton, KeyButton, KeyButton, KeyButton),
    playerId :: EntityId
}

instance Entity Player where

    entityUpdate e s m d =
        let splatter = forM_ m $ \m -> case m of
                MessageCollide _ -> do
                    setSourceRGBA 1 0 0 0.5
                    arc 10 10 30 0 (2 * pi)
                    fill in
        let k = stateKeys s in
        let (keyUp, keyDown, keyLeft, keyRight, keyPrimary, keySecondary) = playerKeys e in
        let (x, y) = playerPosition e in
        let (x', y') = (
                (if keyPressed keyLeft k then x - 80 * d
                else if keyPressed keyRight k then x + 80 * d
                else x),
                (if keyPressed keyUp k then y - 80 * d
                else if keyPressed keyDown k then y + 80 * d
                else y)) in
        let es = if keyPressed keyPrimary k then [entity $ Bullet { 
                bulletPosition = (x' + 20, y' + 20), 
                bulletVelocity = (150, 150),
                bulletTime = 1.0,
                bulletId = entityIdNew }] else [] in
        DeltaState { 
            deltaEntities = entity (e { playerPosition = (x', y') }):es, 
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
        setSourceRGB 1 0 0
        arc 10 10 10 0 (2 * pi)
        fill

    entityOnTop _ = True
    
    entityHitable _ = True

    entityId e = playerId e

    entityChangeId e i = entity (e { playerId = i })

