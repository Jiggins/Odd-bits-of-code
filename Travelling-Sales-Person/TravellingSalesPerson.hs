import Data.Array (Array, array, Ix)
import Data.List.Split (splitOn)
import Data.Matrix (getElem, getRow, matrix, Matrix)
import Data.Vector (minimum, minIndex, (//))
import Prelude hiding (minimum)
import System.IO

import Haversine (haversine, Coords)
import Town

apply :: (a -> b) -> (a, a) -> (b, b)
apply f (a, b) = (f a, f b)

makeTown :: [String] -> Town
makeTown town = Town (townId town) (name town) (coords town) False
	where townId = (read :: String -> Int) . (!! 0)
	      name = (!! 1)
	      coords a = apply (read :: String -> Double) ((a !! 2), (a !! 3))

readTowns :: (Enum i, Num i, Ix i) => FilePath -> IO (Array i Town)
readTowns fileName = do
	f <- openFile fileName ReadMode
	file <- hGetContents f
	return . array (1,80) 
		. (zip [1..80]) . map (makeTown . splitOn "\t") . lines $ file

distanceMatrix :: Monad m => Array Int Town -> m (Matrix Double)
distanceMatrix townList = return .
	matrix 80 80 $ \(i, j) -> haversine (get i) (get j)
		where get x = getCoords x townList

getDistance :: TownId -> TownId -> Matrix a -> a
getDistance x y matrix = getElem x y matrix

-- Horrible hack starts here..
getMin :: (Num a, Ord a) => Path -> Matrix a -> (TownId, a)
getMin p matrix = (minIndex row + 1, minimum row)
	where row = getRow townId matrix // (zip (map (\x -> x-1) $ path p) (repeat 10000))
	      townId = head $ path p

{- Incredibly ugly way of doing Nearest neighbour until I can learn about Mutable Arrays -}
nearestNeighbour :: TownId -> Matrix Double -> Path
nearestNeighbour start matrix = nearest (Path [start] 0) matrix
	where nearest currPath matrix
	        | newDist < 10000 = nearest (Path (newTown : p) (d + newDist)) matrix
	        | otherwise = Path p (d + getDistance first end matrix)
	        where (newTown, newDist) = getMin currPath matrix
	              (p,d) = (path currPath, distance currPath)
	              first = head $ path currPath
	              end   = last $ path currPath

main :: IO ()
main = do
	towns <- readTowns "Towns.csv"
	matrix <- distanceMatrix towns
	print $ nearestNeighbour 18 matrix
