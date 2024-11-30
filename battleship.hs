module Main where

import System.Random

import Setup
import GameLogic
import Input

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

-- Generate a random coordinate that hasn't been shot before
randomCoordinate :: Board -> IO (Maybe (Int, Int))
randomCoordinate board = do
    row <- randomRIO (0, 9)
    col <- randomRIO (0, 9)
    let coord = (row, col)
    return (Just coord)

main :: IO ()
main = do
    let emptyBoard = createBoard 10
    
    playerBoard <- placeShips emptyBoard [5, 4, 3, 3, 2]
    computerBoard <- placeShips emptyBoard [5, 4, 3, 3, 2]
    
    let initialPlayerGameState = GameState playerBoard []
        initialComputerGameState = GameState computerBoard []
    
    putStrLn "Game setup complete! BATTLE SHIP!"
    
    gameLoop initialPlayerGameState initialComputerGameState
    

gameLoop :: GameState -> GameState -> IO ()
gameLoop playerState computerState = do

    putStrLn "\nYour Board:"
    printBoard (board playerState)
    putStrLn "\nComputer's Board:"
    printHiddenBoard (board computerState)
    
    -- Player's turn
    putStrLn "Your turn! Enter coordinates:"
    input <- getLine
    case parseCleanInput input of
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid coordinate in the form of 'B 10'"
            gameLoop playerState computerState
        Just coord -> do
            case validateShot computerState coord of
                Nothing -> do
                    putStrLn "You've already shot at this square. Try again."
                    gameLoop playerState computerState
                Just validCoord -> do
                    (updatedComputerState, shotResult) <- processShot computerState (Just validCoord)
                    case shotResult of
                        Just Hit  -> putStrLn "Hit!"
                        Just Miss -> putStrLn "Miss!"
                        Nothing   -> putStrLn "Something went wrong."
                    
                    -- Check if game is over
                    if isGameOver (board updatedComputerState)
                        then putStrLn "Congratulations! You sunk all the computer's ships!"
                        else do
                            -- Computer's turn (simplified random shot)
                            computerCoord <- randomCoordinate (board playerState)
                            (updatedPlayerState, computerShotResult) <- processShot playerState computerCoord
                            case computerShotResult of
                                Just Hit  -> putStrLn "Computer hit your ship!"
                                Just Miss -> putStrLn "Computer missed."
                                Nothing   -> putStrLn "Something went wrong."
                            
                            -- Check if game is over
                            if isGameOver (board updatedPlayerState)
                                then putStrLn "Game over! Computer sunk all your ships."
                                else gameLoop updatedPlayerState updatedComputerState
