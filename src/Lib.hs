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
import Data.Maybe
import Debug.Trace (trace)

someFunc :: IO ()
someFunc = do
    let cells = evalState (genCells 4 4) (mkStdGen 10)
    loop 100 $ evalState (genCells 4 4) (mkStdGen 10)
    return ()

data Cell = None | Wall | Person deriving (Eq, Show)

data Point = Point 
    { pointCol  :: !Int
    , pointRow  :: !Int
    , pointCell :: !Cell
    } deriving Show

instance Eq Point where
    (==) (Point ax ay _) (Point bx by _) = ax == bx && ay == by

instance Ord Point where
    compare (Point ax ay _) (Point bx by _)
        | a < b = LT
        | b < a = GT
        | otherwise = compare ax bx
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
                next = nextStateMat cells

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

{-
-- canMove :: now -> up -> left -> result
canMove :: Cell -> Cell -> Bool
canMove None _ = True
canMove _ None = True
canMove _ _    = False
-}

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

{-
nextState :: Cells -> Cells
nextState cells = Cells $ nextStateMatrix $ squareWall $ unwrapCells cells
-}

squareWall :: CellMat -> CellMat
squareWall a = Vector.map (addEdge Wall) $ addEdge (Vector.replicate (Vector.length $ Vector.head a) Wall) a

points :: Cells -> [Point]
points cells = List.sort $ join [[Point x y ((cellMat Vector.! y) Vector.! x) | x <- [0..row]] | y <- [0..col]]
    where
        cellMat = unwrapCells cells
        col = (Vector.length cellMat) - 1
        row = (Vector.length $ Vector.head cellMat) - 1

elemAt :: CellMat -> Int -> Int -> Cell
elemAt mat col row = (mat Vector.! row) Vector.! col

data NextState = Down | Right | Stay | Space | NotMove 
    deriving (Eq, Show)

insert :: Cells -> Map.Map Point NextState -> Point -> Map.Map Point NextState
insert cells cont x
    | pointCell x == Wall = insertThis NotMove
    | willEnter && isNone = insertThis enter
    | isNone              = insertThis Space
    | not willMove        = insertThis Stay
    | willEnter           = insertThis enter
    | otherwise           = insertThis Space
        where
            cellMat = unwrapCells cells
            insertThis a = Map.insert x a cont
            enter = isEnter cellMat x
            isNone = pointCell x == None
            willEnter = enter /= Space
            exit = canExit cont x
            willMove = exit == Space

toCell :: NextState -> Cell
toCell Space = None
toCell NotMove = Wall
toCell _ = Person

map2cells :: Int -> Int -> Map.Map Point NextState -> Cells
map2cells col row cont = ret
    where
        genVec :: Int -> CellCol
        genVec index = Vector.generate row (\i -> toCell $ cont Map.! (Point index i None))
        cellMat = Vector.generate col genVec
        ret = Cells cellMat

nextStateMat :: Cells -> Cells
nextStateMat cells = ret
    where
        cellMat = unwrapCells cells
        col = Vector.length cellMat
        row = Vector.length $ Vector.head cellMat
        cont = foldl (insert cells) Map.empty (points cells)
        ret = map2cells col row cont

isEnter :: CellMat -> Point -> NextState
isEnter cells p
    | isCorner = Space
    | isRightEdge = toDown $ pointCell p
    | isDownEdge = toRight $ pointCell p
    | down  == Person = Down
    | right == Person = Lib.Right
    | otherwise = Space
        where
            downX = (+) 1 $ pointCol p
            rightY = (+) 1 $ pointRow p
            isDownEdge = Vector.length cells >= downX
            isRightEdge = Vector.length (Vector.head cells) >= rightY
            isCorner = isDownEdge && isRightEdge
            down  = elemAt cells downX (pointRow p)
            right = elemAt cells (pointCol p) rightY
            toRight :: Cell -> NextState
            toRight Person = Lib.Right
            toRight None = Space
            toDown :: Cell -> NextState
            toDown Person = Down
            toDown None = Space

canExit :: Map.Map Point NextState -> Point -> NextState
canExit cont p
    | isCorner = Stay
    | isLeftEdge = convertTop top
    | isTopEdge = convertLeft left
    | top == Down = Space
    | left == Lib.Right = Space
    | otherwise = Stay
        where
            topX  = (pointCol p) - 1
            leftY = (pointRow p) - 1
            isTopEdge = topX < 0
            isLeftEdge = leftY < 0
            isCorner = isLeftEdge && isTopEdge
            top = cont Map.! (Point topX (pointRow p) None)
            left = cont Map.! (Point (pointCol p) leftY None)
            convertTop :: NextState -> NextState
            convertTop Down = Space
            convertTop _ = Stay
            convertLeft :: NextState -> NextState
            convertLeft Lib.Right = Space
            convertLeft _ = Stay
