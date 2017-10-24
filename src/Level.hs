module Level where

import System.Directory    
import Data.Maybe
import Data.List

data Field = Player | Wall | Point | BigPoint | Enemy | Empty
type Row = [Field]
type Level = ([Row], Points, Position)
type Points = [Bool]
type Position = (Int, Int)

-- Function for converting a Char from the textfile to a Field
textToField :: Char -> Field 
textToField 'P' = Player
textToField 'W' = Wall
textToField '.' = Point
textToField '*' = BigPoint
textToField 'E' = Enemy
textToField ' ' = Empty

findPoints :: String -> Points
findPoints [] = []
findPoints (x:xs)   | x == '.' = [True] ++ findPoints xs
                    | otherwise = []

findPlayerPos :: String -> Position
findPlayerPos s | elemIndex 'P' s == Nothing = error "No Player found in level"    
                | otherwise = (fromJust xCoordinate, fromJust yCoordinate)
    where   rows = lines s
            indices = map (elemIndex 'P') rows
            xCoordinate = fromJust (find isJust indices)
            yCoordinate = elemIndex 'P' (rows !! (fromJust (indices !! (fromJust xCoordinate))))

loadLevel :: FilePath -> IO Level
loadLevel filePath = do
    text <- readFile filePath
    let rows = lines text
    let levelValues = (map . map) textToField rows
    let pointList = findPoints text
    let playerPos = findPlayerPos text
    return $ (levelValues, pointList, playerPos)

getLevels :: FilePath -> IO [FilePath]
getLevels = getDirectoryContents