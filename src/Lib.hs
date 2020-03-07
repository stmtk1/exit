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
squareWall a = Vector.map (addEdge Wall) $ addEdge edgeCol a
    where
        edgeCol = (Vector.// [(0, None)]) $ Vector.replicate (Vector.length $ Vector.head a) Wall

initPoints :: Cells -> Seq.Seq Point
initPoints cells = Seq.fromList $ [Point 0 0 (elemAt cellMat 0 0)]
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
        cellMat = unwrapCells cells
        col = Vector.length cellMat
        row = Vector.length $ Vector.head cellMat
        (cont, _) = insertAndsearch cells (Map.empty, initPoints cells)
        ret = map2cells col row cont

appendPoints :: Cells -> Map.Map Point NextState -> Seq.Seq Point -> Seq.Seq Point
appendPoints cells cont points
    | Prelude.null points = points
    | otherwise  = ret
        where
            next Seq.:<| _ = points
            appended = Seq.fromList $ Maybe.mapMaybe (\t -> appendPoint cells cont next t) [(0, 1), (1, 0), (0, -1), (-1, 0)]
            ret = points Seq.>< appended

appendPoint :: Cells -> Map.Map Point NextState -> Point -> (Int, Int) -> Maybe Point
appendPoint cells cont p (x, y)
    | invalidCol = Nothing
    | invalidRow = Nothing
    | otherwise  = Just (Point newY newX (elemAt cellMat newX newY))
        where
                cellMat = unwrapCells cells
                row = Vector.length cellMat
                col = Vector.length $ Vector.head cellMat
                newX = (pointCol p) + x
                newY = (pointRow p) + y
                invalidCol = newX < 0 || newX >= row
                invalidRow = newY < 0 || newY >= col


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
            toRight _ = Space
            toDown :: Cell -> NextState
            toDown Person = Down
            toDown _ = Space

canExit :: Map.Map Point NextState -> Point -> NextState
canExit cont p
    | isCorner = Space
    | isLeftEdge = convertTop top
    | isTopEdge = Space -- convertLeft left
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
