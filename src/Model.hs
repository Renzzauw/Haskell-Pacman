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
      
initialState :: GameState
initialState = PlayingLevel 0 initialLevel Player { playerPos = initialPlayerPos, playerDir = DirNone} initialPointList []

initialLevel :: Level
initialLevel = undefined

initialPlayerPos :: Position
initialPlayerPos = undefined

initialPointList :: Points
initialPointList = undefined

playerVelocity :: Float
playerVelocity = 0.04

enemyVelocity :: Float
enemyVelocity = 0.04