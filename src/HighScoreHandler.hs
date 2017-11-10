module HighScoreHandler where

import Data.List
import System.IO
import System.Directory

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

getScore :: String -> String
getScore string = whatToRead
    where   whatToRead = words string !! 1

getScores :: String -> [String]
getScores string = map getScore usedRows
    where   rows = lines string
            usedRows = init (tail rows)

newScore :: Int -> String -> [Int]
newScore score string = take 5 (reverse insertedNewScore)
    where   list = getScores string
            ints = map read list :: [Int]
            sortedInts = sort ints
            insertedNewScore = insert score sortedInts

writeHighScore :: Int -> [Int] -> String
writeHighScore score highScores = total
    where   header = "Highscores: "
            s1 = "1. " ++ show (head highScores)
            s2 = "2. " ++ show (highScores !! 1)
            s3 = "3. " ++ show (highScores !! 2)
            s4 = "4. " ++ show (highScores !! 3)
            s5 = "5. " ++ show (highScores !! 4)
            s6 = "Last score: " ++ show score
            total = unlines (header : s1 : s2 : s3 : s4 : s5 : [s6])

createOrOpenHighScores :: IO Handle
createOrOpenHighScores = do
    exists <- doesFileExist "Highscores.txt"
    unless exists $ writeFile "Highscores.txt" startHighScores
    openFile "Highscores.txt" ReadMode
    where   startHighScores = "Highscores:\n1. 0\n2. 0\n3. 0\n4. 0\n5. 0\nLast score: 0"

updateHighScore :: Int -> IO ()
updateHighScore score = do
    h2 <- createOrOpenHighScores
    h <- openFile "HighscoresTEMP.txt" WriteMode
    originalFileContents <- hGetContents h2
    hPutStr h $ writeHighScore score (newScore score originalFileContents)
    hClose h2
    hClose h
    renameFile "HighscoresTEMP.txt" "Highscores.txt"