module Input where

import Data.Char (toUpper)

-- Function to parse input like "b 10" or "B 10" into board coordinates
parseInput :: String -> Maybe (Int, Int)
parseInput input = case words input of
    [col, rowStr]
        | length col == 1 
                     && isValidColumn (toUpper (head col)) 
                     && all (`elem` "0123456789") rowStr ->
            let row = read rowStr
                colIndex = columnToIndex (toUpper (head col))
            in if isValidRow row then Just (row - 1, colIndex) else Nothing
    _ -> Nothing

isValidColumn :: Char -> Bool
isValidColumn c = c `elem` ['A'..'J']

isValidRow :: Int -> Bool
isValidRow r = r >= 1 && r <= 10

-- Convert column letter to index (A = 0, B = 1, ..., J = 9)
columnToIndex :: Char -> Int
columnToIndex c = fromEnum c - fromEnum 'A'

-- Clean input by removing leading/trailing whitespace and converting to uppercase
cleanInput :: String -> String
cleanInput = unwords . words . map toUpper

-- Wrapper function for parsing input that handles cleaning
parseCleanInput :: String -> Maybe (Int, Int)
parseCleanInput = parseInput . cleanInput

-- Test function to read input and parse it
main :: IO ()
main = do
    putStrLn "Enter a coordinate (e.g., B 10):"
    input <- getLine
    let uppercaseInput = unwords $ map (map toUpper) (words input)
    case parseInput uppercaseInput of
        Just (row, col) -> putStrLn $ "Valid coordinate: Row " ++ show row ++ ", Col " ++ show col
        Nothing -> putStrLn "Invalid coordinate! Please try again."
