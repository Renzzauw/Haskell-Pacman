module Level where

import System.Directory  
import System.Random  
import Data.Maybe
import Data.List

data Direction = DirUp | DirDown | DirLeft | DirRight | DirNone
    deriving (Eq)
data FieldType = PlayerField | WallField | PointField | BigPointField | EnemyField | EmptyField | Player2Field
    deriving (Eq)
data EnemyType = GoToPlayer | Random
    deriving (Eq, Enum)
data Player = Player { playerPos :: Position, playerDir :: Direction }
    deriving (Eq)
data Enemy = Enemy { enemyPos :: Position, enemyDir :: Direction, enemyType :: EnemyType }
    deriving (Eq)
type Field = (FieldType, Position)
type Row = [Field]
type Level = [Row]
type Points = [(Position, Bool)]
type Position = (Float, Float)

-- Function for converting a Char from the textfile to a Field
textToField :: Char -> FieldType 
textToField 'P' = PlayerField
textToField 'W' = WallField
textToField '.' = PointField
textToField '*' = BigPointField
textToField 'E' = EnemyField
textToField 'Q' = Player2Field
textToField _ = EmptyField

-- Function that checks if all points have been picked up, if so the player has completed the level
levelComplete :: Points -> Bool
levelComplete = null

-- Function that looks up the player startposition in a given level
findPlayerPos :: String -> Position
findPlayerPos s | index == Nothing = error "No Player found in level"
                | otherwise = (x, y)
    where   rows = lines s
            levelWidth = length (head rows)
            x = fromIntegral ((fromJust index) `mod` levelWidth)
            y = fromIntegral ((fromJust index) `div` levelWidth)
            index = elemIndex 'P' string
            string = filter (/= '\n') s

findPlayer2Pos :: String -> Maybe Position
findPlayer2Pos s | index == Nothing = Nothing
                 | otherwise = Just (x, y)
    where   rows = lines s
            levelWidth = length (head rows)
            x = fromIntegral ((fromJust index) `mod` levelWidth)
            y = fromIntegral ((fromJust index) `div` levelWidth)
            index = elemIndex 'Q' string
            string = filter (/= '\n') s

-- Function that looks up the enemy startpositions in a given level
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

-- Give a list of all the points that the player can collect
findPoints :: String -> Points
findPoints s    | null indices1 && null indices2 = []
                | otherwise = map createPoint indices
    where   rows = lines s
            levelWidth = length (head rows)
            x index = fromIntegral (index `mod` levelWidth)
            y index = fromIntegral (index `div` levelWidth)
            indices1 = elemIndices '.' string
            indices2 = elemIndices '*' string
            indices = sort (indices1 ++ indices2)
            string = filter (/= '\n') s
            createPos index = (x index, y index)
            createPoint index   | elem index indices1 = (createPos index, False)
                                | elem index indices2 = (createPos index, True)

-- Function that loads a level
loadLevel :: FilePath -> IO (Level, Points, Player, Maybe Player, [Enemy])
loadLevel filePath = do
    text <- readFile filePath
    rng <- newStdGen
    let rows = lines text
    let levelValues = createRowsForLevel 0 rows
    let pointList = findPoints text
    let playerPosition = findPlayerPos text
    let player = Player playerPosition DirNone
    let player2Position = findPlayer2Pos text
    let player2 | isNothing player2Position = Nothing
                | otherwise = Just (Player (fromJust player2Position) DirNone)
    let enemyPositions = findEnemyPos text
    let amountOfEnemies = length enemyPositions
    let rngs = createRNGs rng amountOfEnemies
    let enemies = map (\(_rng, _pos) -> createEnemy _rng _pos) $ zip rngs enemyPositions
    return $ (levelValues, pointList, player, player2, enemies)
    where   createEnemy rng pos = Enemy pos DirNone ([GoToPlayer ..] !! (fst (randomR (0 :: Int, 1 :: Int) rng)))

-- Create multiple StdGens, one for each enemy so they are all random types
createRNGs :: StdGen -> Int -> [StdGen]
createRNGs _ 0 = []
createRNGs rng n = newRNG : createRNGs newRNG (n-1)
    where newRNG = snd (randomR (0 :: Float, 1 :: Float) rng)

-- Function that creates the rows
createFieldsForRow :: Float -> Float -> String -> Row
createFieldsForRow _ _ "" = []
createFieldsForRow x y (i:is) = (textToField i, (x, y)) : createFieldsForRow (succ x) y is

-- Function that creates a level out of rows
createRowsForLevel :: Float -> [String] -> Level
createRowsForLevel _ [] = []
createRowsForLevel y (i:is) = createFieldsForRow 0 y i : createRowsForLevel (succ y) is

-- Function that gets all the level files from the levels folder
getLevels :: IO [FilePath]
getLevels = do
            files <- getDirectoryContents "Levels"
            let levels1 = filter (isInfixOf ".txt") files
            let levels2 = take 9 levels1
            return $ sort levels2
