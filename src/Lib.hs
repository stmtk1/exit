module Lib
    ( someFunc
    ) where

import Control.Monad (join)
import System.Random 
import Control.Monad.State.Strict
import Control.Concurrent
import Data.Vector as Vector hiding (foldl, sum)
import qualified Data.List as List
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = do
    loop 100 $ evalState (genCells 4 4) (mkStdGen 10)
    return ()

data Cell = None | Wall | Person deriving Eq

data Point = Point 
    { pointCol  :: !Int
    , pointRow  :: !Int
    , pointCell :: !Cell
    }

instance Eq Point where
    (==) (Point ax ay _) (Point bx by _) = ax == bx && bx == by

instance Ord Point where
    (<=) (Point ax ay _) (Point bx by _)
        | a < b = True
        | b < a = False
        | otherwise = ax <= bx
            where
                a = ax + ay
                b = bx + by

type CellCol = Vector.Vector Cell
type CellMat = Vector.Vector CellCol

newtype Cells = Cells { unwrapCells :: CellMat}
    deriving Eq

instance Show Cells where
    show = showCells

loop :: Int -> Cells -> IO Cells
loop 0 cells = return cells
loop n cells
    | personNum cells == personNum next = do
        putStrLn $ show $ Cells $ squareWall $ unwrapCells cells
        threadDelay (500 * 1000)
        loop (n - 1) $ next
    | otherwise = do
        putStrLn $ show $ Cells $ squareWall $ unwrapCells cells
        putStrLn $ show $ Cells $ squareWall $ unwrapCells next
        return cells
            where
                next = nextState cells

personNum :: Cells -> Int
personNum cells = ret
    where
        isPerson :: Cell -> Bool
        isPerson Person = True
        isPerson _ = False
        personNumColumn :: CellCol -> Int
        personNumColumn col = Vector.length $ Vector.filter isPerson col
        ret = sum $ Vector.map personNumColumn $ unwrapCells cells

genCell :: State StdGen Cell
genCell = (\t -> if t then Person else None) <$> state random

genCells :: Int -> Int -> State StdGen Cells
genCells n m = Cells <$> (Vector.replicateM n $ Vector.replicateM m genCell)

showCells :: Cells -> String
showCells cells = join $ Vector.toList $ Vector.map showCellColumn $ unwrapCells cells

showCellColumn :: CellCol -> String
showCellColumn cells = foldl (\a b -> a List.++ [showCell b]) "\n" $ Vector.toList cells

showCell :: Cell -> Char
showCell Wall = '+'
showCell None = ' '
showCell Person = '*'

-- canMove :: now -> up -> left -> result
canMove :: Cell -> Cell -> Bool
canMove None _ = True
canMove _ None = True
canMove _ _    = False

-- isMoved :: down -> right -> result
isMoved Person _ = True
isMoved _ Person = True
isMoved _ _      = False

nextStateCell :: [Cell] -> [Cell] -> [Cell] -> Cell
nextStateCell _ [_, Wall, _] _ = Wall
nextStateCell _ _ [_, Person, _] = Person
nextStateCell _ [_, _, Person] _ = Person
nextStateCell _ [_, None, _] _ = None
nextStateCell [None, Person, _] _ _ = None
nextStateCell [_, None, _] _ _ = None
nextStateCell [_, Person, _] _ _ = Person
nextStateCell [Person, _, _] _ _ = Person
nextStateCell _ [None, _, _] [None, _, _] = None
nextStateCell _ _ _ = Person

nextStateColumn :: CellCol -> CellCol -> CellCol -> CellCol
nextStateColumn a b c
    | Vector.length a < 3 = Vector.empty
    | otherwise = do
        let nextHead = nextStateCell (Vector.toList $ Vector.take 3 a) (Vector.toList $ Vector.take 3 b) (Vector.toList $ Vector.take 3 c)
            nextTail = nextStateColumn (Vector.tail a) (Vector.tail b) (Vector.tail c)
        Vector.singleton nextHead Vector.++ nextTail

nextStateMatrix :: CellMat -> CellMat
nextStateMatrix mat = Vector.zipWith3 nextStateColumn front now back
    where
        front = Vector.map (mat Vector.!) $ Vector.enumFromN 0 $ (\i -> i - 2) $ Vector.length mat
        now = Vector.map (mat Vector.!) $ Vector.enumFromN 1 $ (\i -> i - 2) $ Vector.length mat
        back = Vector.map (mat Vector.!) $ Vector.enumFromN 2 $ (\i -> i - 2) $ Vector.length mat

addEdge :: a -> Vector.Vector a -> Vector.Vector a
addEdge x xs = (Vector.singleton x Vector.++) $ xs Vector.++ (Vector.singleton x)

nextState :: Cells -> Cells
nextState cells = Cells $ nextStateMatrix $ squareWall $ unwrapCells cells

squareWall :: CellMat -> CellMat
squareWall a = Vector.map (addEdge Wall) $ addEdge (Vector.replicate (Vector.length $ Vector.head a) Wall) a

points :: Cells -> [Point]
points cells = List.sort $ join [[Point x y ((cellMat Vector.! y) Vector.! x) | x <- [0..row]] | y <- [0..col]]
    where
        cellMat = unwrapCells cells
        col = (Vector.length cellMat) - 1
        row = (Vector.length $ Vector.head cellMat) - 1

elemAt :: CellMat -> Int -> Int -> Cell
elemAt mat col row = (mat Vector.! col) Vector.! row

data NextState = Down | Right | Stay | Space | NotMove
    deriving Eq

insert :: Cells -> Point -> Map.Map Point NextState -> Map.Map Point NextState
insert cells x cont
    | pointCell x == Wall              = insertThis cont
    | isEnter cellMat x /= Space       = insertThis (isEnter cellMat x)
    | pointCol x < 1                   = insertThis now
    | up == Space || up == NotMove     = insertThis up
    | pointRow x < 1                   = insertThis now
    | left == Space || left == NotMove = insertThis left
        where
            cellMat = unwrapCells cells
            up = (cont Map.!) $ Point ((-) 1 $ pointCol x) (pointRow x) None
            left = (cont Map.!) $ Point (pointCol x) ((-) 1 $ pointRow x) None
            now = if pointCell x == Person then Stay else Space
            insertThis a = Map.insert x a cont

isEnter :: CellMat -> Point -> NextState
isEnter cells p
    | down  == Person = Down
    | right == Person = Lib.Right
    | otherwise = Space
        where
            down  = elemAt cells ((+) 1 $ pointCol p) (pointRow p)
            right = elemAt cells (pointCol p) ((+) 1 $ pointRow p) 

