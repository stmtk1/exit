module Lib
    ( someFunc
    ) where

import Control.Monad (join)
import System.Random 
import Control.Monad.State.Strict
import Control.Concurrent

someFunc :: IO ()
someFunc = do
    loop 100 $ evalState (genCells 4 4) (mkStdGen 10)
    return ()

data Cell = None | Wall | Person deriving Eq

newtype Cells = Cells { unwrapCells :: [[Cell]]}
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
        personNumColumn :: [Cell] -> Int
        personNumColumn col = length $ filter isPerson col
        ret = sum $ map personNumColumn $ unwrapCells cells

genCell :: State StdGen Cell
genCell = (\t -> if t then Person else None) <$> state random

genCells :: Int -> Int -> State StdGen Cells
genCells n m = Cells <$> (replicateM n $ replicateM m genCell)

showCells :: Cells -> String
showCells cells = join $ map showCellColumn $ unwrapCells cells

showCellColumn :: [Cell] -> String
showCellColumn cells = foldl (\a b -> a ++ [showCell b]) "\n" cells

showCell :: Cell -> Char
showCell Wall = '+'
showCell None = ' '
showCell Person = '*'

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

nextStateColumn :: [Cell] -> [Cell] -> [Cell]  -> [Cell]
nextStateColumn a b c
    | length a < 3 = []
    | otherwise = do
        let nextHead = nextStateCell (take 3 a) (take 3 b) (take 3 c)
            nextTail = nextStateColumn (tail a) (tail b) (tail c)
        (nextHead:nextTail)

nextStateMatrix :: [[Cell]] -> [[Cell]]
nextStateMatrix mat
    | length mat < 3 = []
    | otherwise = do
        let (a1:a2:a3:_) = mat
        (nextStateColumn a1 a2 a3) : nextStateMatrix (tail mat)

addEdge :: a -> [a] -> [a]
addEdge x xs = x : xs ++ [x]

nextState :: Cells -> Cells
nextState cells = Cells $ nextStateMatrix $ squareWall $ unwrapCells cells

squareWall :: [[Cell]] -> [[Cell]]
squareWall a = map (addEdge Wall) $ addEdge [ Wall | _ <- [1..(length $ head a)]] a

