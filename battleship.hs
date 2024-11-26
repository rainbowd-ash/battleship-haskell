module Main where

boardSize :: Int
boardSize = 10

createBoard :: Int -> [[Char]]
createBoard size = replicate size (replicate size '~')

printBoard :: [[Char]] -> IO ()
printBoard board = do
    putStrLn "   A B C D E F G H I J"  -- column labels
    mapM_ printRow (zip [1..] board)

printRow :: (Int, [Char]) -> IO ()
printRow (rowNum, row) = do
    -- align rows
    let label = if rowNum < 10 then " " ++ show rowNum else show rowNum
    putStr (label ++ " ")
    putStrLn (unwords (map (:[]) row))

-- Main function to run the board setup
main :: IO ()
main = do
    let board = createBoard boardSize
    printBoard board
