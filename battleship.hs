module Main where

import System.Random (randomRIO)
import Control.Monad (foldM)

type Board = [[Char]]

boardSize :: Int
boardSize = 10

ships :: [Int]
ships = [5, 4, 3, 3, 2]

createBoard :: Int -> Board
createBoard size = replicate size (replicate size '~')

placeShips :: Board -> IO Board
placeShips board = foldM placeShip board ships

placeShip :: Board -> Int -> IO Board
placeShip board shipLength = do
    let positions = [(row, col) | row <- [0..boardSize-1], col <- [0..boardSize-1]]
    direction <- randomDirection
    tryPlaceShip board shipLength direction positions

randomDirection :: IO Bool
randomDirection = randomRIO (True, False) -- True = horizontal, False = vertical

tryPlaceShip :: Board -> Int -> Bool -> [(Int, Int)] -> IO Board
tryPlaceShip board shipLength horizontal positions = do
    start <- randomRIO (0, length positions - 1)
    let (row, col) = positions !! start
    if canPlaceShip board shipLength horizontal (row, col)
        then return $ placeShipOnBoard board shipLength horizontal (row, col)
        else tryPlaceShip board shipLength horizontal positions

canPlaceShip :: Board -> Int -> Bool -> (Int, Int) -> Bool
canPlaceShip board shipLength horizontal (row, col)
    | horizontal = col + shipLength <= boardSize && all (isWater row) [col..col+shipLength-1]
    | otherwise  = row + shipLength <= boardSize && all (`isWater` col) [row..row+shipLength-1]
  where
    isWater r c = board !! r !! c == '~'

placeShipOnBoard :: Board -> Int -> Bool -> (Int, Int) -> Board
placeShipOnBoard board shipLength horizontal (row, col)
    | horizontal = foldl (\b c -> updateBoard b row c 'S') board [col..col+shipLength-1]
    | otherwise  = foldl (\b r -> updateBoard b r col 'S') board [row..row+shipLength-1]

updateBoard :: Board -> Int -> Int -> Char -> Board
updateBoard board row col val =
    take row board ++ [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "  A B C D E F G H I J"
    mapM_ printRow (zip [1..] board)

printRow :: (Int, [Char]) -> IO ()
printRow (rowNum, row) = do
    -- align rows
    let label = if rowNum < 10 then " " ++ show rowNum else show rowNum
    putStr (label ++ " ")
    putStrLn (unwords (map (:[]) row))

-- Main function to test ship placement
main :: IO ()
main = do
    let board = createBoard boardSize
    finalBoard <- placeShips board
    printBoard finalBoard
