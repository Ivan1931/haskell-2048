module TwentyFortyEight where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

top = 3

type X = Int
type Y = Int
type Coord = (X, Y)
type Square = Maybe Int
type Board = Map.Map Coord Square

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

--Warning, this will overide adjasent blocks. Use with canShiftSquare
shiftSquare :: Board -> Coord -> Board
shiftSquare board xy@(x,y) 
  | x == top          = board
  | isNothing current = board
  | cantShift         = board
  | otherwise         = blankLast $ shiftToNext
  where current     = fromJust $ Map.lookup xy board
        cantShift   = not $ canShiftSquare board xy 
        next        = (x + 1, y)
        shiftToNext = Map.adjust (\_ -> current) next board
        blankLast   = Map.adjust (\_ -> Nothing) xy

canShiftSquare :: Board -> Coord -> Bool
canShiftSquare board (x,y)
  | x == top             = False
  | isNothing nextSquare = True
  | otherwise            = False
  where nextSquare       = fromJust $ Map.lookup (x + 1,y) board
                                       
shiftColumn :: Board -> Board
shiftColumn board = iterateShits board (3,3)
  where iterateShits board' xy@(x,y) = let nextBoard = shiftSquare board' xy
                                       in case xy of
                                         (0,0) -> nextBoard
                                         (0,_) -> iterateShits nextBoard (3, y - 1)
                                         (_,_) -> iterateShits nextBoard (x - 1, y)

--hehehehehehe
fullShift :: Board -> Board
fullShift = shiftColumn . shiftColumn . shiftColumn . shiftColumn


canMergeSquare :: Board -> Coord -> Bool
canMergeSquare board xy@(x,y) = (current == next) && (not $ isNothing next) && (not $ isNothing $ fromJust next)
    where getSquare coord = Map.lookup coord board
          current = getSquare xy
          next = getSquare (x + 1, y)

merge :: Board -> Board
merge _ = undefined

rotate :: Board -> Board
rotate _ = undefined
