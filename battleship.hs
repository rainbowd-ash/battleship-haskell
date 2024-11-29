module Main where

import Setup

debug :: Bool
debug = False

-- Print related stuff
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "  A B C D E F G H I J"
    mapM_ printRow (zip [1..] board)

printHiddenBoard :: Board -> IO ()
printHiddenBoard board = do
    putStrLn "  A B C D E F G H I J"
    mapM_ printRow (zip [1..] board)
  where
    printRow (rowNum, row) =
        putStrLn $ (if rowNum < 10 then " " else "") ++ show rowNum ++ " " ++ unwords (map showHiddenSquare row)

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

showHiddenSquare :: Square -> String
showHiddenSquare Water = "~"
showHiddenSquare Ship  = "~"  -- Hide ships in hidden view
showHiddenSquare Hit   = "X"
showHiddenSquare Miss  = "O"

--

main :: IO ()
main = do
    let emptyBoard = createBoard 10
    
    playerBoard <- placeShips emptyBoard [5, 4, 3, 3, 2]
    computerBoard <- placeShips emptyBoard [5, 4, 3, 3, 2]
    
    -- Display boards based on debug mode
    if debug
        then do
            putStrLn "Player Board (Debug Mode):"
            printBoard playerBoard
            putStrLn "Computer Board (Debug Mode):"
            printBoard computerBoard
        else do
            putStrLn "Player Board:"
            printBoard playerBoard
            putStrLn "Computer Board:"
            printHiddenBoard computerBoard
    
    putStrLn "Game setup complete! BATTLE SHIP!"
