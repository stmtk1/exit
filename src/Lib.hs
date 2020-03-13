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
import Data.Maybe as Maybe
import Debug.Trace (trace)
import qualified Data.Sequence as Seq

someFunc :: IO ()
someFunc = do
    loop 200 $ evalState (genCells 4 4) (mkStdGen 10)
    return ()

data Cell = None | Wall | Person deriving (Eq, Show)

data Point = Point 
    { pointCol  :: !Int
    , pointRow  :: !Int
    , pointCell :: !Cell
    } deriving Show

data Direction = Up | Right | Left | Down
    deriving (Show, Eq)

data NextState = Defined { nextStateDirection :: Direction } | Stay | Space | NotMove 
    deriving (Eq, Show)

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
loop n cells = do
        putStrLn $ show $ Cells $ unwrapCells cells
        threadDelay (500 * 1000)
        loop (n - 1) $ next
            where
                next = nextState cells

genCell :: State StdGen Cell
genCell = (\t -> if t then Person else None) <$> state random

genCells :: Int -> Int -> State StdGen Cells
genCells n m = Cells <$> squareWall <$> (Vector.replicateM m $ Vector.replicateM n genCell)

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
-- squareWall a = Vector.imap (\i m -> addEdge (isWall i) m) $ addEdge edgeCol a
squareWall a = Vector.map (addEdge Wall) $ addEdge edgeCol a
    where
        edgeCol = (Vector.// [(0, None)]) $ Vector.replicate (Vector.length $ Vector.head a) Wall
        -- isWall i = if i == 2 then None else Wall

initPoints :: Cells -> Seq.Seq Point
-- initPoints cells = Seq.fromList $ [Point 0 0 (cellsElem cells 0 0), Point 0 2 (cellsElem cells 0 2)]
initPoints cells = Seq.fromList $ [Point 0 0 (cellsElem cells 0 0)]
    where
        col = cellsColSize cells - 1
        row = cellsRowSize cells - 1

elemAt :: CellMat -> Int -> Int -> Cell
elemAt mat col row = (mat Vector.! row) Vector.! col


insert :: Cells -> Map.Map Point NextState -> Point -> Map.Map Point NextState
insert cells cont x
    | pointCell x == Wall = insertThis NotMove
    | willEnter && isNone = insertThis enter
    | isNone              = insertThis Space
    | not willMove        = insertThis Stay
    | willEnter           = insertThis enter
    | otherwise           = insertThis Space
        where
            insertThis a = Map.insert x a cont
            enter = isEnter cells cont x
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
        ret = Cells $ Vector.generate col genVec

insertAndsearch :: Cells -> (Map.Map Point NextState, Seq.Seq Point) -> (Map.Map Point NextState, Seq.Seq Point)
insertAndsearch cells (cont, points)
    | Prelude.null points = (cont, points)
    | isJust $ cont Map.!? next = insertAndsearch cells (cont, rest)
    | otherwise = insertAndsearch cells (insert cells cont next, appendPoints cells cont points)
        where
            next Seq.:<| rest = points

nextState :: Cells -> Cells
nextState cells = ret
    where
        col = cellsColSize cells
        row = cellsRowSize cells
        (cont, _) = insertAndsearch cells (Map.empty, initPoints cells)
        ret = map2cells col row cont

appendPoints :: Cells -> Map.Map Point NextState -> Seq.Seq Point -> Seq.Seq Point
appendPoints cells cont points
    | Prelude.null points = points
    | otherwise  = ret
        where
            next Seq.:<| _ = points
            appended = Seq.fromList $ Maybe.mapMaybe (\t -> appendPoint cells cont next t) [Lib.Left, Lib.Right, Up, Down]
            ret = points Seq.>< appended

appendPoint :: Cells -> Map.Map Point NextState -> Point -> Direction -> Maybe Point
appendPoint cells cont p dir
    | invalid = Nothing
    | otherwise = Just (Point newY newX (cellsElem cells newY newX))
        where
                col = cellsColSize cells
                row = cellsRowSize cells
                newPoint = addDiff p dir
                invalid = not $ validPoint cells newPoint
                newY = pointCol newPoint
                newX = pointRow newPoint

validPoint :: Cells -> Point -> Bool
validPoint cells (Point x y _) = 
    x >= 0 && y >= 0 && x < colSize && y < rowSize
        where
            colSize = cellsColSize cells
            rowSize = cellsRowSize cells

addDiff :: Point -> Direction -> Point
addDiff (Point col row cell) dir = Point (col + dx) (row + dy) cell
    where
        (dx, dy) = toDiff dir

isEnter :: Cells -> Map.Map Point NextState -> Point -> NextState
isEnter cells cont p
    | isCorner                = Space
    | isRightEdge             = toDown down
    | isDownEdge && isRightUp = Space
    | isDownEdge              = toRight right
    | (down == Person)        = Defined Down
    | isRightUp               = Space
    | right == Person         = Defined Lib.Right
    | otherwise               = Space
        where
            downY = (+) 1 $ pointRow p
            upY = (pointRow p) - 1
            rightX = (+) 1 $ pointCol p
            isDownEdge = not $ haveDirection cells p Down
            isRightEdge = not $ haveDirection cells p Lib.Right
            hasRightUp = upY >= 0 && not isRightEdge
            isRightUp = hasRightUp && cont Map.! (Point rightX upY None) == Defined Down
            isCorner = isDownEdge && isRightEdge
            down  = cellsElem cells (pointCol p) downY
            right = cellsElem cells  rightX (pointRow p)
            toRight :: Cell -> NextState
            toRight Person = Defined Lib.Right
            toRight _ = Space
            toDown :: Cell -> NextState
            toDown Person = Defined Down
            toDown _ = Space

cellsElem :: Cells -> Int -> Int -> Cell
cellsElem cells col row = elemAt (unwrapCells cells) col row

haveDirection :: Cells -> Point -> Direction -> Bool
haveDirection cells p dir
    = validPoint cells $ addDiff p dir
    {-
    = x >= 0 && y >= 0 && x < colSize && y < rowSize
        where
            Point x y _ = addDiff p dir
            colSize = cellsColSize cells
            rowSize = cellsRowSize cells
-}
toDiff :: Direction -> (Int, Int)
toDiff Down = (0, 1)
toDiff Up = (0, -1)
toDiff Lib.Left = (-1, 0)
toDiff Lib.Right = (1, 0)

{-
definedDirection :: CellMat -> Map.Map Point NextState -> Point -> NextState
definedDirection cells cont p
    | haveRight = (-)
    | isRight
-}

cellsColSize :: Cells -> Int
cellsColSize = Vector.length . Vector.head . unwrapCells

cellsRowSize :: Cells -> Int
cellsRowSize = Vector.length . Vector.head . unwrapCells

canExit :: Map.Map Point NextState -> Point -> NextState
canExit cont p
    | isCorner = Space
    | isLeftEdge = convertTop top
    | isTopEdge = Space -- convertLeft left
    | top == Defined Down = Space
    | left == Defined Lib.Right = Space
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
            convertTop (Defined Down) = Space
            convertTop _ = Stay
            convertLeft :: NextState -> NextState
            convertLeft (Defined Lib.Right) = Space
            convertLeft _ = Stay
