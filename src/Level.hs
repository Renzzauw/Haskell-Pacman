module Level where

import System.Directory    
import Data.Maybe
import Data.List

data Direction = DirUp | DirDown | DirLeft | DirRight | DirNone
    deriving (Eq)
data Field = PlayerField | WallField | PointField | BigPointField | EnemyField | EmptyField
    deriving (Eq)
data Player = Player { playerPos :: Position, playerDir :: Direction }
    deriving (Eq)
data Enemy = Enemy { enemyPos :: Position, enemyDir :: Direction }
    deriving (Eq)
type Row = [Field]
type Level = [Row]
type Points = [Bool]
type Position = (Int, Int)

-- Function for converting a Char from the textfile to a Field
textToField :: Char -> Field 
textToField 'P' = PlayerField
textToField 'W' = WallField
textToField '.' = PointField
textToField '*' = BigPointField
textToField 'E' = EnemyField
textToField ' ' = EmptyField

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

loadLevel :: FilePath -> IO (Level, Points, Position)
loadLevel filePath = do
    text <- readFile filePath
    let rows = lines text
    let levelValues = (map . map) textToField rows
    let pointList = findPoints text
    let playerPos = findPlayerPos text
    return $ (levelValues, pointList, playerPos)

getLevels :: FilePath -> IO [FilePath]
getLevels = getDirectoryContents