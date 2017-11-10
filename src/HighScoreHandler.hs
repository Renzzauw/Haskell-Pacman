module HighScoreHandler where

import Data.List

-- The high score file looks like this:
{-
Highscores:
1. 0
2. 0
3. 0
4. 0
5. 0
Last score: 0
-}

getScore :: String -> Int
getScore string = read ((words string) !! 1)

getScores :: String -> [Int]
getScores string = map getScore usedRows
    where   rows = lines string
            usedRows = tail rows

newScore :: Int -> [Int] -> [Int]
newScore score list = take 5 newList
    where   newList = insert score list

writeHighScore :: Int -> [Int] -> String
writeHighScore score highScores = total
    where   header = "Highscores: "
            s1 = "1. " ++ show (highScores !! 0)
            s2 = "2. " ++ show (highScores !! 1)
            s3 = "3. " ++ show (highScores !! 2)
            s4 = "4. " ++ show (highScores !! 3)
            s5 = "5. " ++ show (highScores !! 4)
            s6 = "Last score: " ++ show score
            total = unlines (header : s1 : s2 : s3 : s4 : s5 : [s6])

updateHighScore :: Int -> IO ()
updateHighScore score = do
    text <- readFile "Highscores.txt"
    length text `seq` (writeFile "Highscores.txt" $ writeHighScore score (newScore score (getScores text)))