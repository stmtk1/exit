{-# OPTIONS_GHC -Wunused-foralls #-}

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
    loop 20 $ evalState (genCells 20 21) (mkStdGen 10)
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
        | otherwise = compare ay by
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
genCells n m = Cells <$> (Vector.replicateM m $ Vector.replicateM n genCell)

showCells :: Cells -> String
showCells cells = join $ Vector.toList $ Vector.map showCellColumn $ unwrapCells cells

showCellColumn :: CellCol -> String
showCellColumn cells = foldl (\a b -> a List.++ [showCell b]) "\n" $ Vector.toList cells

showCell :: Cell -> Char
showCell Wall = '+'
showCell None = ' '
showCell Person = '*'

addEdge :: a -> Vector.Vector a -> Vector.Vector a
addEdge x xs = (Vector.singleton x Vector.++) $ xs Vector.++ (Vector.singleton x)

squareWall :: CellMat -> CellMat
squareWall a = Vector.map (addEdge Wall) $ addEdge (Vector.replicate (Vector.length $ Vector.head a) Wall) a

points :: Cells -> [Point]
points cells = List.sort $ join [[Point x y (elemAt cellMat x y) | x <- [0..col]] | y <- [0..row]]
    where
        cellMat = unwrapCells cells
        col = (Vector.length $ Vector.head cellMat) - 1
        row = (Vector.length cellMat) - 1

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
            enter = isEnter cellMat cont x
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
        genVec index = Vector.generate row (\i -> toCell $ cont Map.! (Point i index None))
        cellMat = Vector.generate col genVec
        ret = Cells cellMat

nextState :: Cells -> Cells
nextState cells = ret
    where
        cellMat = unwrapCells cells
        col = Vector.length cellMat
        row = Vector.length $ Vector.head cellMat
        cont = foldl (insert cells) Map.empty (points cells)
        ret = map2cells col row cont

isEnter :: CellMat -> Map.Map Point NextState -> Point -> NextState
isEnter cells cont p
    | isCorner                = Space
    | isRightEdge             = toDown down
    | isDownEdge && isRightup = Space
    | isDownEdge              = toRight right
    | (down == Person)        = Down
    | isRightup               = Space
    | right == Person         = Lib.Right
    | otherwise               = Space
        where
            downY = (+) 1 $ pointRow p
            upY = (pointRow p) - 1
            rightX = (+) 1 $ pointCol p
            isDownEdge = Vector.length cells <= downY
            isRightEdge = Vector.length (Vector.head cells) <= rightX
            isCorner = isDownEdge && isRightEdge
            down  = elemAt cells (pointCol p) downY
            right = elemAt cells  rightX (pointRow p)
            hasRightUp = upY >= 0 && (not isRightEdge)
            isRightup = hasRightUp && cont Map.! (Point rightX upY None) == Down
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
            topY  = (pointRow p) - 1
            leftX = (pointCol p) - 1
            isTopEdge = topY < 0
            isLeftEdge = leftX < 0
            isCorner = isLeftEdge && isTopEdge
            top = cont Map.! (Point (pointCol p) topY None)
            left = cont Map.! (Point leftX (pointRow p) None)
            convertTop :: NextState -> NextState
            convertTop Down = Space
            convertTop _ = Stay
            convertLeft :: NextState -> NextState
            convertLeft Lib.Right = Space
            convertLeft _ = Stay
