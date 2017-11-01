module Level where

import System.Directory    
import Data.Maybe
import Data.List

data Direction = DirUp | DirDown | DirLeft | DirRight | DirNone
    deriving (Eq)
data FieldType = PlayerField | WallField | PointField | BigPointField | EnemyField | EmptyField
    deriving (Eq)
data Player = Player { playerPos :: Position, playerDir :: Direction }
    deriving (Eq)
data Enemy = Enemy { enemyPos :: Position, enemyDir :: Direction }
    deriving (Eq)
type Field = (FieldType, Position)
type Row = [Field]
type Level = [Row]
type Points = [Bool]
type Position = (Float, Float)

-- Function for converting a Char from the textfile to a Field
textToField :: Char -> FieldType 
textToField 'P' = PlayerField
textToField 'W' = WallField
textToField '.' = PointField
textToField '*' = BigPointField
textToField 'E' = EnemyField
textToField ' ' = EmptyField

-- Give a list of all the points that the player can collect
findPoints :: String -> Points
findPoints [] = []
findPoints (x:xs)   | x == '.' || x == '*' = [True] ++ findPoints xs
                    | otherwise = []

levelComplete :: [Bool] -> Bool
levelComplete xs = null xs

findPlayerPos :: String -> Position
findPlayerPos s | index == Nothing = error "No Player found in level"
                | otherwise = (x, y)
    where   rows = lines s
            levelWidth = length (head rows)
            x = fromIntegral ((fromJust index) `mod` levelWidth)
            y = fromIntegral ((fromJust index) `div` levelWidth)
            index = elemIndex 'P' string
            string = filter (/= '\n') s

findEnemyPos :: String -> [Position]
findEnemyPos s  | null indices = []
                | otherwise = map createPos indices
    where   rows = lines s
            levelWidth = length (head rows)
            x index = fromIntegral (index `mod` levelWidth)
            y index = fromIntegral (index `div` levelWidth)
            indices = elemIndices 'E' string
            string = filter (/= '\n') s
            createPos index = (x index, y index)

loadLevel :: FilePath -> IO (Level, Points, Player, [Enemy])
loadLevel filePath = do
    text <- readFile filePath
    let rows = lines text
    --let levelValues = (map . map) textToField rows
    let levelValues = createRowsForLevel 0 rows
    let pointList = findPoints text
    let playerPosition = findPlayerPos text
    let player = Player playerPosition DirNone
    let enemyPositions = findEnemyPos text
    let enemies = map createEnemy enemyPositions
    return $ (levelValues, pointList, player, enemies)
    where createEnemy pos = Enemy pos DirNone

createFieldsForRow :: Float -> Float -> String -> Row
createFieldsForRow _ _ "" = []
createFieldsForRow x y (i:is) = (textToField i, (x, y)) : createFieldsForRow (succ x) y is

createRowsForLevel :: Float -> [String] -> Level
createRowsForLevel _ [] = []
createRowsForLevel y (i:is) = createFieldsForRow 0 y i : createRowsForLevel (succ y) is

getLevels :: IO [FilePath]
getLevels = do
            files <- getDirectoryContents "../Levels"
            let levels = filter (isInfixOf ".txt") files
            return $ sort levels
