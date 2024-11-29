module Main where
import Setup

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
