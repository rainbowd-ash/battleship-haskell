module GameLogic where

import Setup
import Input

-- Game state to track selected squares
data GameState = GameState {
    board :: Board,
    selectedSquares :: [(Int, Int)]
}

-- Check if a square has already been selected
isSquareSelected :: GameState -> (Int, Int) -> Bool
isSquareSelected gameState coord = coord `elem` selectedSquares gameState

-- Update the board based on the shot result
updateBoardAfterShot :: Board -> Int -> Int -> Board
updateBoardAfterShot board row col =
    case board !! row !! col of
        Water -> updateBoard board row col Miss
        Ship  -> updateBoard board row col Hit
        _     -> board  -- If already Hit or Miss, no change

-- Check if a shot is valid (not previously selected)
validateShot :: GameState -> (Int, Int) -> Maybe (Int, Int)
validateShot gameState coord
    | coord `elem` selectedSquares gameState = Nothing
    | otherwise = Just coord

-- Process a shot, returning updated game state and shot result
processShot :: GameState -> Maybe (Int, Int) -> IO (GameState, Maybe Square)
processShot gameState Nothing = return (gameState, Nothing)
processShot gameState (Just (row, col)) = do
    let updatedBoard = updateBoardAfterShot (board gameState) row col
    let updatedSelectedSquares = (row, col) : selectedSquares gameState
    let result = case updatedBoard !! row !! col of
                   Hit  -> Just Hit
                   Miss -> Just Miss
                   _    -> Nothing
    return (GameState updatedBoard updatedSelectedSquares, result)

-- Check if all ships are sunk
isGameOver :: Board -> Bool
isGameOver = not . any (any (== Ship))