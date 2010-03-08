module Image (imageWithAll) where
import Graphics.Rendering.Cairo
import System.FilePath
import System.Directory
import Control.Monad
import qualified Data.Map as Map

imagePath = "../images/"

imageWithAll :: ((String -> Surface) -> IO a) -> IO a
imageWithAll f = do
    is <- getDirectoryContents imagePath
    is' <- filterM (\i -> doesFileExist (imagePath ++ i)) is
    images Map.empty ([i | i <- is', takeExtension i == ".png"])
    where
        images m [] = f (imageRequire m)
        images m (i:is) = withImageSurfaceFromPNG (imagePath ++ i) (\s -> images (Map.insert i s m) is)

imageRequire :: Map.Map String Surface -> String -> Surface
imageRequire m i = case Map.lookup i m of
    Just s -> s
    Nothing -> error ("Image '" ++ i ++ "' was not found.")

