module Main where

import System.Random (randomRIO)
import Control.Monad (foldM)

type Board = [[Square]]

data Square = Water | Ship | Hit | Miss
    deriving (Eq, Show)

boardSize :: Int
boardSize = 10

ships :: [Int]
ships = [5, 4, 3, 3, 2]

createBoard :: Int -> Board
createBoard size = replicate size (replicate size Water)

-- Ship placement stuff
placeShip :: Board -> [(Int, Int)] -> Board
placeShip = foldl (\b (row, col) -> updateBoard b row col Ship)

placeShips :: Board -> [Int] -> IO Board
placeShips = foldM placeRandomShip

placeRandomShip :: Board -> Int -> IO Board
placeRandomShip board length = do
    dir <- randomRIO (True, False)  -- True = Horizontal, False = Vertical
    row <- randomRIO (0, 9)
    col <- randomRIO (0, 9)
    let positions = shipPositions row col length dir
    if isValidPlacement board positions
        then return (placeShip board positions)
        else placeRandomShip board length  -- Retry if invalid

isValidPlacement :: Board -> [(Int, Int)] -> Bool
isValidPlacement board = all (\(row, col) -> inBounds row col && board !! row !! col == Water)

inBounds :: Int -> Int -> Bool
inBounds row col = row >= 0 && row < 10 && col >= 0 && col < 10

shipPositions :: Int -> Int -> Int -> Bool -> [(Int, Int)]
shipPositions row col length isHorizontal =
    if isHorizontal
        then [(row, col + i) | i <- [0..length-1]]
        else [(row + i, col) | i <- [0..length-1]]

-- 

updateBoard :: Board -> Int -> Int -> Square -> Board
updateBoard board row col val =
    take row board ++
    [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

-- Print related stuff
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "  A B C D E F G H I J"
    mapM_ printRow (zip [1..] board)

printRow :: (Int, [Square]) -> IO ()
printRow (rowNum, row) = do
    let label = if rowNum < 10 then " " ++ show rowNum else show rowNum
    putStr (label ++ " ")
    putStrLn (unwords (map showSquare row))

showSquare :: Square -> String
showSquare Water = "~"
showSquare Ship  = "#"
showSquare Hit   = "X"
showSquare Miss  = "O"

--

main :: IO ()
main = do
    let emptyBoard = createBoard 10
    boardWithShips <- placeShips emptyBoard [5, 4, 3, 3, 2]
    printBoard boardWithShips
