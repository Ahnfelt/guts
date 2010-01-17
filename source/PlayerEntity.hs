module PlayerEntity where
import Graphics.Rendering.Cairo
import GameState

data Player = Player { 
    playerPosition :: Position 
}

instance Entity Player where

    entityUpdate e s d =
        let (x, y) = playerPosition e
            e' = e { playerPosition = (x + 100 * d, y) }
        in DeltaState { deltaEntities = [entity e'], deltaSplatter = return () }
    
    entityPosition e = Just (playerPosition e)

    entityDraw e = do
        setSourceRGB 1 0 0
        arc 0 0 10 0 (2 * pi)
        fill

    entityOnTop _ = True

