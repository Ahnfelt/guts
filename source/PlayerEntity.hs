module PlayerEntity where
import GameState

-- The actual type is defined in GameState because GameState
-- uses the Player type (and this module imports GameState).

instance Entity Player where
    entityPosition e = Just (playerPosition e)

