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
      | LevelChooser 
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
        deriving (Eq)
      
initialState :: IO GameState
initialState = do
    (level, points, player, enemies) <- loadLevel "Levels/Level02.txt"
    let state = PlayingLevel 0 level player points enemies
    return $ state

playerVelocity :: Float
playerVelocity = 0.04

enemyVelocity :: Float
enemyVelocity = 0.04