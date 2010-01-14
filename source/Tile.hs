module Tile (Tile (..), tileSolid, tileWidth, tileHeight, TilePainter, TileMap, tileGet, tileSet, tileMapEmpty, tileMap, tileCoordinates) where
import Graphics.Rendering.Cairo (Render)
import Data.Array.Diff
import Data.List

-- The different sorts of tiles
data Tile
    = OutdoorGrass
    | OutdoorTree
    | OutdoorLake
    | OutdoorRoad
    | BaseWall
    | BaseFloor
    | BaseGrid
    | Abyss
    deriving Eq

-- Determines wether or not a tile is solid
tileSolid :: Tile -> Bool
tileSolid OutdoorTree = True
tileSolid OutdoorLake = True
tileSolid BaseWall = True
tileSolid _ = False

tileWidth = 30
tileHeight = 30

-- A painter is a function that may draw a tile based on the tile and surrounding tiles.
-- The map is painted in scanlines (rows) from left to right and top to bottom, and the
-- painter may draw wherever it likes (but it typically only makes sense to draw in the
-- vicinity of the provided coordinates, the top left position of the center tile)
type TilePainter = 
    Tile -> -- center
    (Tile, Tile, Tile, Tile) -> -- north, south, west, east
    (Tile, Tile, Tile, Tile) -> -- northWest, northEast, southWest, southEast
    Int -> Int -> -- x y
    Int -> -- random seed
    Maybe (Render ())

newtype TileMap = TileMap (DiffArray (Int, Int) Tile)

tileGet :: TileMap -> Int -> Int -> Tile
tileGet (TileMap a) x y = a ! (x, y)

tileSet :: TileMap -> Int -> Int -> Tile -> TileMap
tileSet (TileMap a) x y t = TileMap (a // [((x, y), t)])

tileMap :: [[Char]] -> TileMap
tileMap l = TileMap $ listArray ((0, 0), (length (head l) - 1, length l - 1)) (map tile $ concat $ transpose l)
    where
        tile '*' = OutdoorTree
        tile _ = OutdoorGrass

tileMapEmpty :: Int -> Int -> TileMap
tileMapEmpty w h = TileMap (listArray ((0, 0), (w - 1, h - 1)) $ take (w * h) $ repeat Abyss)

tileCoordinates :: TileMap -> [(Int, Int)]
tileCoordinates (TileMap a) = indices a

