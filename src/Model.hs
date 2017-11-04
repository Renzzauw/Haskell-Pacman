-- | This module contains the data types
--   which represent the state of the game
module Model where
  
import Level 

-- Data type for each state the game is currently in
data GameState = 
      PlayingLevel {
          score :: Int,
          level :: Level,
          player :: Player,
          pointList :: Points,
          enemies :: [Enemy] } 
      | MainMenu 
      | LevelChooser {
          levels :: [FilePath] }
      | DiedScreen {
          score :: Int }
      | WonScreen {
          score :: Int }
      | Paused {
          score :: Int,
          level :: Level,
          player :: Player,
          pointList :: Points,
          enemies :: [Enemy] }
      | HelpScreen
        deriving (Eq)
      
initialState :: FilePath -> IO GameState
initialState filePath = do
    (level, points, player, enemies) <- loadLevel ("Levels/" ++ filePath)
    let state = PlayingLevel 0 level player points enemies
    return $ state

levelChooserState :: IO GameState
levelChooserState = do
    _levels <- getLevels
    let state = LevelChooser _levels
    return $ state

playerVelocity :: Float
playerVelocity = 0.04

enemyVelocity :: Float
enemyVelocity = 0.04