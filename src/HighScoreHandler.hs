module HighScoreHandler where

import Data.List

-- The high score file looks like this:
{-
Highscores:
1. score
2. score
3. score
4. score
5. score
-}

-- Function that gets the current high score data from the file
getHighScoreData :: IO [String]
getHighScoreData = do
    text <- readFile "Highscores.txt"
    let rows = lines text
    return rows

getScore :: String -> Int
getScore string = read ((words string) !! 1)

getScores :: IO [String] -> IO [Int]
getScores string = do
    strings <- string
    let scores = map getScore (tail strings)
    return scores

newScore :: Int -> [Int] -> [Int]
newScore score list = take 5 newList
    where   newList = insert score list ++ [0, 0, 0, 0, 0]

writeHighScore :: [Int] -> IO ()
writeHighScore highScores = writeFile "Highscores.txt" total
    where   header = "Highscores: "
            s1 = "1. " ++ show (highScores !! 0)
            s2 = "2. " ++ show (highScores !! 1)
            s3 = "3. " ++ show (highScores !! 2)
            s4 = "4. " ++ show (highScores !! 3)
            s5 = "5. " ++ show (highScores !! 4)
            total = unlines (header : s1 : s2 : s3 : s4 : [s5])

updateHighScore :: Int -> IO ()
updateHighScore score = do
    scores <- getScores getHighScoreData
    let newScores = newScore score scores
    length scores `seq` writeHighScore newScores