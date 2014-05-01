module TwentyFortyEight where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Applicative as A
import Data.Maybe
import Control.Monad
import System.Random
import Control.Concurrent (threadDelay)

empty = fromString "****\n\
                   \****\n\
                   \****\n\
                   \****"
top = 3

two = Just (2 :: Int)
four = Just (4 :: Int)
atStart = set empty ((0,0), Just 2)

type X = Int
type Y = Int
type Coord = (X, Y)
type Square = Maybe Int
type Board = Map.Map Coord Square
type Operation = (Board -> Coord -> Board)
type Predicate = (Board -> Coord -> Bool)

data Move = U | D | L | R
          deriving (Eq, Show, Read)

type Strategy = (Board -> IO Move)

toString :: Board -> String
toString board = iterate "" (0,0)
  where iterate xs xy@(x,y) = let square = fromJust $ Map.lookup xy board 
                                  c      = if isNothing square
                                           then "*"
                                           else show (fromJust square)
                                  next   = xs ++ "\t" ++ c
                              in case xy of
                                (3,3) -> next
                                (3,_) -> iterate (next ++ "\n\n") (0, y + 1)
                                (_,_) -> iterate next (x + 1, y)

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
                                       mergeNext = Map.adjust (\a -> (fmap (+) next) A.<*> a) xy'' board'
                                       blankLast = Map.adjust (\_ -> Nothing) xy' 
                                   in blankLast $ mergeNext

merge :: Board -> Board
merge board = applyToAll board mergeSquare 

transition :: Board -> Board
transition board = fullShift $ merge $ fullShift board

rotateCoord :: Coord -> Coord
rotateCoord (x,y) = rot
  where toFloat a = fromIntegral a :: Float
        toInt   a = floor a :: Int
        rot = let x' = toFloat x - 1.5
                  y' = toFloat y - 1.5
              in (toInt ((-y') + 1.5), toInt (x' + 1.5))

rotate :: Board -> Board
rotate board = Map.mapWithKey rotate' empty
  where rotate' xy _ = fromJust $ Map.lookup (rotateCoord xy) board

canPlaceSquare :: Board -> Bool
canPlaceSquare board = (transition board) /= board

isEmpty :: Maybe (Maybe a) -> Bool
isEmpty = isNothing . fromJust

--rotates board to a right orientation from specified other orientation
rotateToRightFromLeft = rotate . rotate
rotateToRightFromUp = rotate
rotateToRightFromDown = rotate . rotate . rotate 

--rotates board to a specified orientation from right orientation
rotateToDownFromRight = rotate
rotateToUpFromRight = rotate . rotate .rotate
rotateToLeftFromRight = rotate . rotate

--hehehehehehehehehe
leftTransition  = rotateToRightFromLeft . transition . rotateToLeftFromRight
rightTransition = transition
upTransition    = rotateToRightFromUp . transition . rotateToUpFromRight
downTransition  = rotateToRightFromDown . transition . rotateToDownFromRight

countEmptySquares :: Board -> Int
countEmptySquares board  = Map.foldl' count 0 board
  where count acc square = if isNothing square then acc + 1 else acc

actionForMove :: Move -> (Board -> Board)
actionForMove L  = leftTransition
actionForMove R = rightTransition
actionForMove U    = upTransition
actionForMove D  = downTransition

--finds all possible spawn points on the right of the board
possibleSpawnCoords :: Board -> [Coord]
possibleSpawnCoords board = checkEnd 0
  where checkEnd idy = let takeIfEmpty = if isEmpty $ Map.lookup (top, idy) board then [(top, idy)] else []
                       in case idy of 
                                3 -> takeIfEmpty
                                _ -> takeIfEmpty ++ checkEnd (idy + 1)

spawnsLeft :: Board -> [Coord]
spawnsLeft  board = map (rotateCoord . rotateCoord ) $ possibleSpawnCoords $ rotateToRightFromLeft board
spawnsRight :: Board -> [Coord]
spawnsRight board = possibleSpawnCoords board
spawnsDown :: Board -> [Coord]
spawnsDown    board = map rotateCoord $ possibleSpawnCoords $ rotateToRightFromUp board
spawnsUp :: Board -> [Coord]
spawnsUp  board = map (rotateCoord . rotateCoord . rotateCoord) $ possibleSpawnCoords $ rotateToRightFromDown board

--finds all possible spawn coords on the board
allPossibleSpawnPoints :: Board -> [Coord]
allPossibleSpawnPoints board = forLeft ++ forUp ++ forDown ++ forRight
  where forLeft  = spawnsLeft board
        forUp    = spawnsUp board
        forDown  = spawnsDown board
        forRight = spawnsRight board

hasLost :: Board -> Bool
hasLost board = allTransitions board == board && allPossibleSpawnPoints board == []
  where allTransitions = leftTransition . rightTransition . upTransition . downTransition

score :: Board -> Int
score board = Map.foldl (\acc v -> acc + (squareScore v)) 0 board
  where squareScore Nothing  = 0
        squareScore (Just 2) = 0
        squareScore (Just a) = f 1 $ reverse $ takeWhile (<= a) [2 ^ i | i <- [2..]]
        f _ [] = 0
        f c (x: xs) = (c * x) + f (c * 2) xs

randomStrategy :: Board -> IO Move
randomStrategy _ = do
    g <- getStdGen
    let randoms = randomRs (0 :: Int, 3 :: Int) g
        move = case head $ randoms of
                  0 -> L
                  1 -> R
                  2 -> U
                  3 -> D
    return move

randomSpawnPoint :: Board -> Move -> IO Board
randomSpawnPoint board move = do
    newStdGen
    g <- getStdGen
    let getCoord = case move of
                         U -> spawnsDown
                         D -> spawnsUp
                         R -> spawnsLeft
                         L -> spawnsRight
        nextSquare = case randomR (False, True) g of
                       (True,_) -> (Just 4)
                       _    -> (Just 2)
        spawnCoords = getCoord board
        coordIndex = fst (randomR (0, (length spawnCoords) - 1) g)
    return $ case spawnCoords of
               [] -> board
               --this appaling line is selecting a random value from the
               --returned possible coordinates
               xs -> set board (spawnCoords !! coordIndex, nextSquare)

iterateGame :: Board -> Strategy -> IO (Board, Bool)
iterateGame board applyStrategy = do 
    nextMove <- applyStrategy board
    let applyMove = actionForMove nextMove
        board'    = applyMove board
        lost      = hasLost board'
    board'' <- randomSpawnPoint board' nextMove
    if board' /= board
    then return (board'', lost)
    else return (board', lost)
    

standardGameDriver :: Board -> Strategy -> IO Board
standardGameDriver board applyStrategy = do
    (board', lost) <- iterateGame board applyStrategy
    if lost 
    then return $ board'
    else standardGameDriver board' applyStrategy

--main = standardGameDriver atStart randomStrategy >>= (\a -> putStrLn $ toString a)
