module PlayerEntity where
import Graphics.Rendering.Cairo
import Control.Monad
import GameState
import KeyState
import Mechanics
import Message

data Player = Player { 
    playerPosition :: Position,
    playerId :: EntityId
}

instance Entity Player where

    entityUpdate e s m d =
        let splatter = forM_ m $ \m -> case m of
                MessageCollide _ -> do
                    setSourceRGBA 1 0 0 0.5
                    arc 0 0 30 0 (2 * pi)
                    fill in
        let k = stateKeys s in
        let (x, y) = playerPosition e in
        let (x', y') = (
                (if keyPressed "Left" k then x - 80 * d
                else if keyPressed "Right" k then x + 80 * d
                else x),
                (if keyPressed "Up" k then y - 80 * d
                else if keyPressed "Down" k then y + 80 * d
                else y))
        in DeltaState { 
            deltaEntities = [entity (e { playerPosition = (x', y') })], 
            deltaMessages = [],
            deltaSplatter = do
                splatter
                setSourceRGBA 0 0 0 0.03
                arc 0 0 10 0 (2 * pi)
                fill
        }
    
    entityPosition e = Just (playerPosition e)

    entityDraw e = do
        setSourceRGB 1 0 0
        arc 0 0 10 0 (2 * pi)
        fill

    entityOnTop _ = True

    entityId e = playerId e

    entityChangeId e i = entity (e { playerId = i })

