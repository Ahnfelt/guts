module Tile (
    Tile (..), OutdoorTile (..), BaseTile (..), 
    TileLike (..), tileSolid, tileWidth, tileHeight, 
    TilePainter, TileMap, tileMapEmpty, tileMap, 
    tileAt, tileGet, tileSet, 
    tileCoordinates, tileMapWidth, tileMapHeight
    ) where
import Graphics.Rendering.Cairo (Render)
import Data.Array.Diff
import Data.List

tileWidth = 32
tileHeight = 32

data OutdoorTile
    = OutdoorGrass
    | OutdoorBush
    | OutdoorTree
    | OutdoorLake
    | OutdoorRoad
    | OutdoorRock
    deriving Eq

data BaseTile
    = BaseWall
    | BaseFloor
    | BaseGrid
    | BaseBlock
    deriving Eq

data Tile
    = TileOutdoor OutdoorTile
    | TileBase BaseTile
    | TileAbyss

-- Determines wether or not a tile is solid
tileSolid :: Tile -> Bool
tileSolid t = tileLike [OutdoorTree, OutdoorLake] t || tileLike [BaseWall] t

-- Determines whether or not a tile is the same as another modulo revealing
class TileLike a where
    tileLike :: a -> Tile -> Bool

instance (TileLike a) => TileLike [a] where
    tileLike l t = any (\a -> tileLike a t) l

instance TileLike OutdoorTile where
    tileLike a (TileOutdoor t) = a == t
    tileLike _ _ = False

instance TileLike BaseTile where
    tileLike a (TileBase t) = a == t
    tileLike _ _ = False

instance TileLike Tile where
    tileLike t' (TileBase t) = tileLike t t'
    tileLike t' (TileOutdoor t) = tileLike t t'
    tileLike TileAbyss TileAbyss = True
    tileLike _ _ = False

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
    Render ()

newtype TileMap = TileMap (DiffArray (Int, Int) Tile)

tileGet :: TileMap -> Int -> Int -> Tile
tileGet (TileMap a) x y = let ((x1, y1), (x2, y2)) = bounds a in
    if x >= x1 && y >= y1 && x <= x2 && y <= y2 then a ! (x, y) else TileAbyss

tileSet :: TileMap -> Int -> Int -> Tile -> TileMap
tileSet (TileMap a) x y t = let ((x1, y1), (x2, y2)) = bounds a in
    if x >= x1 && y >= y1 && x <= x2 && y <= y2 then TileMap (a // [((x, y), t)]) else TileMap a

tileMap :: [[Char]] -> TileMap
tileMap l = TileMap $ listArray ((0, 0), (length (head l) - 1, length l - 1)) (map tile $ concat $ transpose l)
    where
        tile 'b' = TileBase BaseBlock
        tile '*' = TileOutdoor OutdoorGrass
        tile ' ' = TileOutdoor OutdoorBush
        tile '`' = TileOutdoor OutdoorRock
        tile _ = TileOutdoor OutdoorRock

tileMapEmpty :: Int -> Int -> TileMap
tileMapEmpty w h = TileMap (listArray ((0, 0), (w - 1, h - 1)) $ take (w * h) $ repeat TileAbyss)

tileMapWidth :: TileMap -> Int
tileMapWidth (TileMap a) = 1 + (fst $ snd $ bounds a)
tileMapHeight :: TileMap -> Int
tileMapHeight (TileMap a) = 1 + (snd $ snd $ bounds a)

tileCoordinates :: TileMap -> [(Int, Int)]
tileCoordinates (TileMap a) = indices a

tileAt :: TileMap -> Int -> Int -> Tile
tileAt m x y = tileGet m (x `div` tileWidth) (y `div` tileHeight)

