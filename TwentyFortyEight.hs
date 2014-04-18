module TwentyFortyEight where

import qualified Data.Map as Map
import qualified Data.List as List
import Control.Applicative
import Data.Maybe

top = 3
two = Just (2 :: Int)
four = Just (4 :: Int)

type X = Int
type Y = Int
type Coord = (X, Y)
type Square = Maybe Int
type Board = Map.Map Coord Square
type Operation = (Board -> Coord -> Board)
type Predicate = (Board -> Coord -> Bool)

toString :: Board -> String
toString _ = undefined

fromString :: String -> Board
fromString str = Map.fromList [((i,j), (getSquare i j)) | i <- [0 .. top], j <- [0 .. top]]
  where rows = lines str
        charAt a b = (rows !! a) !! b
        getSquare a b = let c = charAt a b
                        in if c == '*' then Nothing else Just (read [c] :: Int)

set :: Board -> (Coord, Square) -> Board
set board (xy, square) = Map.adjust (\_ -> square) xy board

setCoords :: Board -> [(Coord, Square)] -> Board
setCoords board list = List.foldl' step board list
  where step board' xy = set board' xy

applyToSquare :: Board -> Operation -> Predicate -> Coord -> Board
applyToSquare board op pred xy@(x,y) 
  | x == top          = board
  | isNothing current = board
  | cant              = board
  | otherwise         = op board xy
  where current     = fromJust $ Map.lookup xy board
        cant        = not $ pred board xy 

canShiftSquare :: Board -> Coord -> Bool
canShiftSquare board (x,y)
  | x == top             = False
  | isNothing nextSquare = True
  | otherwise            = False
  where nextSquare       = fromJust $ Map.lookup (x + 1,y) board

shiftSquare :: Board -> Coord -> Board
shiftSquare board xy = applyToSquare board shiftToNext canShiftSquare xy
  where shiftToNext board' xy'@(x,y) = let next        = (x + 1, y)
                                           current     = fromJust $ Map.lookup xy board'
                                           shiftToNext = Map.adjust (\_ -> current) next board'
                                           blankLast   = Map.adjust (\_ -> Nothing) xy'
                                        in blankLast $ shiftToNext

applyToAll :: Board -> Operation -> Board
applyToAll board op = iterate board (3,3)
  where iterate board' xy@(x,y) = let nextBoard = op board' xy
                                       in case xy of
                                         (0,0) -> nextBoard
                                         (0,_) -> iterate nextBoard (3, y - 1)
                                         (_,_) -> iterate nextBoard (x - 1, y)
                                       
--performs shift on each square
shift :: Board -> Board
shift board = applyToAll board shiftSquare

--performs shift on each square four times
fullShift :: Board -> Board
fullShift = shift . shift . shift . shift


canMergeSquare :: Board -> Coord -> Bool
canMergeSquare board xy@(x,y) = (current == next) && (not $ isNothing next) && (not $ isNothing $ fromJust next)
    where getSquare coord = Map.lookup coord board
          current = getSquare xy
          next = getSquare (x + 1, y)

mergeSquare :: Board -> Coord -> Board
mergeSquare board xy = applyToSquare board doMerge canMergeSquare xy
  where doMerge board' xy'@(x,y) = let xy'' = (x + 1, y)
                                       next = fromJust $ Map.lookup xy'' board'
                                       current = fromJust $ Map.lookup xy' board' 
                                       mergeNext = Map.adjust (\a -> (fmap (+) next) <*> a) xy'' board'
                                       blankLast = Map.adjust (\_ -> Nothing) xy' 
                                   in blankLast $ mergeNext

merge :: Board -> Board
merge board = applyToAll board mergeSquare 

transition :: Board -> Board
transition board = fullShift $ merge $ fullShift board

rotate :: Board -> Board
rotate _ = undefined
